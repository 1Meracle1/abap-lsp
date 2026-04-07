import * as fs from "fs";
import * as http from "http";
import * as https from "https";
import * as path from "path";
import * as vscode from "vscode";

const SAP_PASSWORD_SECRET_PREFIX = "abap-ls.sapPassword";

export interface SapConnectionConfig {
	baseUrl: string;
	username: string;
	password: string;
}

export interface AdtObjectRef {
	uri: string;
	type: string;
	name: string;
	packageName: string;
	description: string;
}

export interface AdtDependencyFetchResult {
	body: string;
	fileExtension: "abap" | "xml";
	manifestKind: string;
}

interface HttpResponseData {
	statusCode: number;
	headers: http.IncomingHttpHeaders;
	body: string;
}

interface GetSapConnectionOptions {
	promptIfMissing?: boolean;
}

interface AdtClientOptions {
	beforeRequest?: () => Promise<void>;
}

export async function getSapConnectionConfig(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
	options: GetSapConnectionOptions = {},
): Promise<SapConnectionConfig | undefined> {
	const promptIfMissing = options.promptIfMissing ?? true;
	const config = vscode.workspace.getConfiguration("abap-ls", workspaceFolder.uri);
	const storedBaseUrl = (config.get<string>("sap.baseUrl") ?? "").trim();
	const storedUsername = (config.get<string>("sap.username") ?? "").trim();
	const secretKey = secretKeyForWorkspace(workspaceFolder);
	const storedPassword = (await context.secrets.get(secretKey)) ?? "";

	let baseUrl = storedBaseUrl;
	if (!baseUrl) {
		if (!promptIfMissing) {
			return undefined;
		}
		baseUrl = (await vscode.window.showInputBox({
			prompt: "SAP base URL",
			placeHolder: "https://host.example.com",
			ignoreFocusOut: true,
		}))?.trim() ?? "";
		if (!baseUrl) {
			return undefined;
		}
		await config.update("sap.baseUrl", baseUrl, vscode.ConfigurationTarget.WorkspaceFolder);
	}

	let username = storedUsername;
	if (!username) {
		if (!promptIfMissing) {
			return undefined;
		}
		username = (await vscode.window.showInputBox({
			prompt: "SAP username",
			ignoreFocusOut: true,
		}))?.trim() ?? "";
		if (!username) {
			return undefined;
		}
		await config.update("sap.username", username, vscode.ConfigurationTarget.WorkspaceFolder);
	}

	let password = storedPassword;
	if (!password) {
		if (!promptIfMissing) {
			return undefined;
		}
		password = (await vscode.window.showInputBox({
			prompt: "SAP password",
			password: true,
			ignoreFocusOut: true,
		})) ?? "";
		if (!password) {
			return undefined;
		}
		await context.secrets.store(secretKey, password);
	}

	return {
		baseUrl: normalizeBaseUrl(baseUrl),
		username,
		password,
	};
}

export function isSupportedDependencyObject(objectRef: AdtObjectRef, kindHint?: string): boolean {
	const loweredType = objectRef.type.toUpperCase();
	const loweredUri = objectRef.uri.toLowerCase();

	switch (kindHint) {
		case "include":
			return loweredUri.includes("/programs/includes/") || loweredType === "PROG/I";
		case "static":
			return loweredUri.includes("/oo/classes/") ||
				loweredUri.includes("/oo/interfaces/") ||
				loweredType.startsWith("CLAS/") ||
				loweredType.startsWith("INTF/");
		case "type":
			return isDdicDependencyObject(objectRef) ||
				loweredUri.includes("/oo/classes/") ||
				loweredUri.includes("/oo/interfaces/") ||
				loweredType.startsWith("CLAS/") ||
				loweredType.startsWith("INTF/");
	}

	return loweredUri.includes("/programs/includes/") ||
		loweredUri.includes("/programs/programs/") ||
		loweredUri.includes("/oo/classes/") ||
		loweredUri.includes("/oo/interfaces/") ||
		loweredUri.includes("/functions/groups/") ||
		isDdicDependencyObject(objectRef) ||
		loweredType === "PROG/I" ||
		loweredType === "PROG/P" ||
		loweredType.startsWith("CLAS/") ||
		loweredType.startsWith("INTF/");
}

export function pickBestDependencyObject(
	query: string,
	objects: AdtObjectRef[],
	kindHint?: string,
): AdtObjectRef | undefined {
	const normalizedQuery = query.trim().toLowerCase();
	if (!normalizedQuery) {
		return undefined;
	}

	const supported = objects.filter((objectRef) => isSupportedDependencyObject(objectRef, kindHint));
	if (supported.length === 0) {
		return undefined;
	}

	const exactMatches = supported.filter((objectRef) => objectRef.name.trim().toLowerCase() === normalizedQuery);
	if (exactMatches.length > 0) {
		return exactMatches[0];
	}

	return supported[0];
}

export async function configureSapConnection(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
): Promise<void> {
	const config = vscode.workspace.getConfiguration("abap-ls", workspaceFolder.uri);
	const currentBaseUrl = (config.get<string>("sap.baseUrl") ?? "").trim();
	const currentUsername = (config.get<string>("sap.username") ?? "").trim();

	const baseUrl = (await vscode.window.showInputBox({
		prompt: "SAP base URL",
		value: currentBaseUrl,
		ignoreFocusOut: true,
	}))?.trim();
	if (!baseUrl) {
		return;
	}

	const username = (await vscode.window.showInputBox({
		prompt: "SAP username",
		value: currentUsername,
		ignoreFocusOut: true,
	}))?.trim();
	if (!username) {
		return;
	}

	const password = await vscode.window.showInputBox({
		prompt: "SAP password",
		password: true,
		ignoreFocusOut: true,
	});
	if (!password) {
		return;
	}

	await config.update("sap.baseUrl", normalizeBaseUrl(baseUrl), vscode.ConfigurationTarget.WorkspaceFolder);
	await config.update("sap.username", username, vscode.ConfigurationTarget.WorkspaceFolder);
	await context.secrets.store(secretKeyForWorkspace(workspaceFolder), password);
	vscode.window.showInformationMessage("SAP connection saved for this workspace.");
}

export class AdtClient {
	private csrfToken = "";
	private cookies: string[] = [];

	constructor(
		private readonly connection: SapConnectionConfig,
		private readonly options: AdtClientOptions = {},
	) {}

	async searchRepositoryObjects(query: string, maxResults: number = 51): Promise<AdtObjectRef[]> {
		await this.ensureSession();

		const response = await this.request(
			`/sap/bc/adt/repository/informationsystem/search?operation=quickSearch&query=${encodeURIComponent(query)}&maxResults=${maxResults}`,
			{
				headers: {
					Accept: "application/xml",
					"Cache-Control": "no-cache",
					"x-csrf-token": this.csrfToken,
				},
			},
		);

		return parseObjectReferences(response.body);
	}

	async fetchObjectSource(objectUri: string): Promise<string> {
		await this.ensureSession();

		const normalizedUri = objectUri.endsWith("/source/main")
			? objectUri
			: `${objectUri}/source/main`;

		const response = await this.request(normalizedUri, {
			headers: {
				Accept: "text/plain",
				"Cache-Control": "no-cache",
				"x-csrf-token": this.csrfToken,
			},
		});

		return response.body;
	}

	async fetchDependencyObject(objectRef: AdtObjectRef): Promise<AdtDependencyFetchResult> {
		if (isDdicDependencyObject(objectRef)) {
			const ddicKind = inferDdicManifestKind(objectRef);
			const body = await this.fetchDdicObject(ddicKind, objectRef.name);
			return {
				body,
				fileExtension: "xml",
				manifestKind: ddicKind,
			};
		}

		return {
			body: await this.fetchObjectSource(objectRef.uri),
			fileExtension: "abap",
			manifestKind: inferRepositoryManifestKind(objectRef),
		};
	}

	async fetchDdicObject(
		kind: "ddic-data-element" | "ddic-structure" | "ddic-table" | "ddic-table-type" | "ddic-view",
		name: string,
	): Promise<string> {
		await this.ensureSession();
		const encodedName = encodeURIComponent(name);
		let path: string;
		let accept: string;
		if (kind === "ddic-data-element") {
			path = `/sap/bc/adt/ddic/dataelements/${encodedName}`;
			accept = "application/vnd.sap.adt.dataelements.v1+xml, application/vnd.sap.adt.dataelements.v2+xml";
		} else {
			path = `/sap/bc/adt/ddic/elementinfo?path=${encodedName}`;
			accept = "application/vnd.sap.adt.elementinfo+xml";
		}

		const response = await this.request(path, {
			headers: {
				Accept: accept,
				"Cache-Control": "no-cache",
				"x-csrf-token": this.csrfToken,
			},
		});
		return formatDdicXml(response.body);
	}

	async cacheRemoteObject(
		workspaceFolder: vscode.WorkspaceFolder,
		objectRef: AdtObjectRef,
		source: string,
		fileExtension: "abap" | "xml" = "abap",
	): Promise<void> {
		const cacheRoot = path.join(workspaceFolder.uri.fsPath, ".abapls", "cache");
		const objectsDir = path.join(cacheRoot, "objects");
		await fs.promises.mkdir(objectsDir, { recursive: true });

		const slug = encodeURIComponent(objectRef.name);
		const metadataPath = path.join(objectsDir, `${slug}.json`);

		await fs.promises.writeFile(
			metadataPath,
			JSON.stringify(
				{
					...objectRef,
					fileExtension,
					size: source.length,
					fetchedAt: new Date().toISOString(),
				},
				null,
				2,
			),
			"utf8",
		);
	}

	private async ensureSession(): Promise<void> {
		if (this.csrfToken) {
			return;
		}

		const response = await this.request("/sap/bc/adt/runtime/systemmessages", {
			headers: {
				Accept: "application/xml",
				"Cache-Control": "no-cache",
				"x-csrf-token": "Fetch",
			},
		});

		const tokenHeader = response.headers["x-csrf-token"];
		const token = Array.isArray(tokenHeader) ? tokenHeader[0] : tokenHeader;
		this.csrfToken = token ?? "";
		const rawCookies = response.headers["set-cookie"] ?? [];
		this.cookies = rawCookies.map((cookie) => cookie.split(";", 1)[0]);

		if (!this.csrfToken) {
			throw new Error("SAP ADT did not return a CSRF token.");
		}
	}

	private async request(
		pathOrUrl: string,
		options: {
			method?: string;
			headers?: Record<string, string>;
		} = {},
	): Promise<HttpResponseData> {
		await this.options.beforeRequest?.();

		const url = toAbsoluteUrl(this.connection.baseUrl, pathOrUrl);
		const parsed = new URL(url);
		const client = parsed.protocol === "https:" ? https : http;

		const headers: Record<string, string> = {
			Authorization: `Basic ${Buffer.from(`${this.connection.username}:${this.connection.password}`, "utf8").toString("base64")}`,
			...options.headers,
		};
		if (this.cookies.length > 0) {
			headers.Cookie = this.cookies.join("; ");
		}

		return new Promise<HttpResponseData>((resolve, reject) => {
			const request = client.request(
				parsed,
				{
					method: options.method ?? "GET",
					headers,
				},
				(response) => {
					const chunks: Buffer[] = [];
					response.on("data", (chunk: Buffer | string) => {
						chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk));
					});
					response.on("end", () => {
						const body = Buffer.concat(chunks).toString("utf8");
						const statusCode = response.statusCode ?? 0;
						if (statusCode >= 400) {
							reject(new Error(`ADT request failed (${statusCode}): ${body}`));
							return;
						}
						resolve({
							statusCode,
							headers: response.headers,
							body,
						});
					});
				},
			);

			request.on("error", reject);
			request.end();
		});
	}
}

export function isDdicDependencyObject(objectRef: AdtObjectRef): boolean {
	const type = objectRef.type.toUpperCase();
	return type === "DTEL/DE" ||
		type === "TABL/DS" ||
		type === "TABL/DT" ||
		type === "TABL/DA" ||
		type === "TTYP/DA" ||
		type === "VIEW/DV";
}

export function inferDdicManifestKind(
	objectRef: AdtObjectRef,
): "ddic-data-element" | "ddic-structure" | "ddic-table" | "ddic-table-type" | "ddic-view" {
	switch (objectRef.type.toUpperCase()) {
		case "DTEL/DE":
			return "ddic-data-element";
		case "TABL/DS":
			return "ddic-structure";
		case "TABL/DT":
			return "ddic-table";
		case "TABL/DA":
		case "TTYP/DA":
			return "ddic-table-type";
		case "VIEW/DV":
			return "ddic-view";
		default:
			return "ddic-structure";
	}
}

function inferRepositoryManifestKind(objectRef: AdtObjectRef): string {
	const loweredUri = objectRef.uri.toLowerCase();
	if (loweredUri.includes("/programs/includes/") || objectRef.type === "PROG/I") {
		return "include";
	}
	if (loweredUri.includes("/oo/classes/") || objectRef.type.startsWith("CLAS/")) {
		return "global-class";
	}
	if (loweredUri.includes("/oo/interfaces/") || objectRef.type.startsWith("INTF/")) {
		return "global-interface";
	}
	if (loweredUri.includes("/functions/groups/")) {
		return "function-group";
	}
	return "report";
}

function normalizeBaseUrl(baseUrl: string): string {
	return baseUrl.replace(/\/+$/, "");
}

function toAbsoluteUrl(baseUrl: string, pathOrUrl: string): string {
	if (/^https?:\/\//i.test(pathOrUrl)) {
		return pathOrUrl;
	}
	return `${normalizeBaseUrl(baseUrl)}${pathOrUrl.startsWith("/") ? "" : "/"}${pathOrUrl}`;
}

function secretKeyForWorkspace(workspaceFolder: vscode.WorkspaceFolder): string {
	return `${SAP_PASSWORD_SECRET_PREFIX}:${workspaceFolder.uri.toString()}`;
}

function parseObjectReferences(xml: string): AdtObjectRef[] {
	const results: AdtObjectRef[] = [];
	const objectRefRegex = /<adtcore:objectReference\b([^>]*)\/>/g;
	for (const match of xml.matchAll(objectRefRegex)) {
		const attributes = match[1] ?? "";
		results.push({
			uri: decodeXmlEntity(readAttribute(attributes, "adtcore:uri")),
			type: decodeXmlEntity(readAttribute(attributes, "adtcore:type")),
			name: decodeXmlEntity(readAttribute(attributes, "adtcore:name")),
			packageName: decodeXmlEntity(readAttribute(attributes, "adtcore:packageName")),
			description: decodeXmlEntity(readAttribute(attributes, "adtcore:description")),
		});
	}
	return results.filter((entry) => entry.uri && entry.name);
}

function readAttribute(attributes: string, name: string): string {
	const escapedName = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	const match = attributes.match(new RegExp(`${escapedName}="([^"]*)"`, "i"));
	return match?.[1] ?? "";
}

function decodeXmlEntity(value: string): string {
	return value
		.replace(/&quot;/g, "\"")
		.replace(/&apos;/g, "'")
		.replace(/&lt;/g, "<")
		.replace(/&gt;/g, ">")
		.replace(/&amp;/g, "&");
}

export function formatDdicXml(xml: string): string {
	const trimmed = xml.trim();
	if (!trimmed.startsWith("<")) {
		return xml;
	}

	const tokens = trimmed
		.replace(/>\s+</g, "><")
		.split(/(<[^>]+>)/g)
		.map((part) => part.trim())
		.filter((part) => part.length > 0);

	const lines: string[] = [];
	let indent = 0;
	for (const token of tokens) {
		if (!token.startsWith("<")) {
			lines.push(`${"  ".repeat(indent)}${token}`);
			continue;
		}

		if (token.startsWith("</")) {
			indent = Math.max(indent - 1, 0);
			lines.push(`${"  ".repeat(indent)}${token}`);
			continue;
		}

		if (token.startsWith("<?") || token.startsWith("<!")) {
			lines.push(`${"  ".repeat(indent)}${token}`);
			continue;
		}

		const selfClosing = token.endsWith("/>");
		lines.push(`${"  ".repeat(indent)}${token}`);
		if (!selfClosing) {
			indent += 1;
		}
	}

	return `${lines.join("\n")}\n`;
}
