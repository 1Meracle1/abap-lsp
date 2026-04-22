import * as fs from "fs";
import * as http from "http";
import * as https from "https";
import * as path from "path";
import * as vscode from "vscode";

const SAP_PASSWORD_SECRET_PREFIX = "abap-ls.sapPassword";
// Newer ADT backends expose system messages as an Atom feed, while older
// systems still accept generic XML for the same CSRF bootstrap request.
export const SESSION_BOOTSTRAP_ACCEPT = "application/atom+xml;type=feed, application/xml";

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
	sharedDependencies?: AdtDependencyArtifact[];
}

export interface AdtDependencyArtifact {
	objectRef: AdtObjectRef;
	body: string;
	fileExtension: "abap" | "xml";
	manifestKind: string;
}

export interface AdtRepositoryChild {
	objectRef: AdtObjectRef;
	categoryTag: string;
	objectTypeLabel: string;
	expandable: boolean;
}

interface HttpResponseData {
	statusCode: number;
	headers: http.IncomingHttpHeaders;
	body: string;
}

interface RepositoryNodeStructure {
	treeContent: RepositoryNodeEntry[];
	objectTypes: RepositoryObjectTypeInfo[];
}

interface RepositoryNodeEntry {
	objectType: string;
	objectName: string;
	objectUri: string;
	objectVitUri: string;
	expandable: boolean;
}

interface RepositoryObjectTypeInfo {
	objectType: string;
	categoryTag: string;
	label: string;
	nodeId: string;
}

interface GetSapConnectionOptions {
	promptIfMissing?: boolean;
}

interface AdtClientOptions {
	beforeRequest?: () => Promise<void>;
	isCancelled?: () => boolean;
}

const SAP_BASE_URL_ENV_KEYS = ["ABAP_ADT_URL", "ABAP_ADT_BASE_URL", "SAPBASE_URL"] as const;
const SAP_USERNAME_ENV_KEYS = ["ABAP_ADT_USER", "ABAP_ADT_USERNAME", "SAPUSER"] as const;
const SAP_PASSWORD_ENV_KEYS = ["ABAP_ADT_PASSWORD", "SAPPASS"] as const;

export class AdtRequestCancelledError extends Error {
	constructor(message = "ADT request cancelled.") {
		super(message);
		this.name = "AdtRequestCancelledError";
	}
}

export function parseDotenvContents(content: string): Map<string, string> {
	const values = new Map<string, string>();
	for (const rawLine of content.split(/\r?\n/u)) {
		const line = rawLine.trim();
		if (!line || line.startsWith("#")) {
			continue;
		}
		const separator = line.indexOf("=");
		if (separator <= 0) {
			continue;
		}
		const key = line.slice(0, separator).trim();
		if (!key) {
			continue;
		}
		let value = line.slice(separator + 1).trim();
		if (!value) {
			values.set(key, "");
			continue;
		}
		if ((value.startsWith("\"") && value.endsWith("\"")) || (value.startsWith("'") && value.endsWith("'"))) {
			value = value.slice(1, -1);
		} else {
			const commentIndex = value.indexOf("#");
			if (commentIndex >= 0) {
				value = value.slice(0, commentIndex).trimEnd();
			}
		}
		values.set(key, value);
	}
	return values;
}

export function resolveSapConnectionDefaults(
	env: NodeJS.ProcessEnv,
	dotenv: ReadonlyMap<string, string>,
): Partial<SapConnectionConfig> {
	return {
		baseUrl: firstConnectionValue(SAP_BASE_URL_ENV_KEYS, env, dotenv),
		username: firstConnectionValue(SAP_USERNAME_ENV_KEYS, env, dotenv),
		password: firstConnectionValue(SAP_PASSWORD_ENV_KEYS, env, dotenv),
	};
}

function firstConnectionValue(
	keys: readonly string[],
	env: NodeJS.ProcessEnv,
	dotenv: ReadonlyMap<string, string>,
): string | undefined {
	for (const key of keys) {
		const fromEnv = normalizedNonEmpty(env[key]);
		if (fromEnv) {
			return fromEnv;
		}
	}
	for (const key of keys) {
		const fromDotenv = normalizedNonEmpty(dotenv.get(key));
		if (fromDotenv) {
			return fromDotenv;
		}
	}
	return undefined;
}

async function loadSapConnectionDefaults(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
): Promise<Partial<SapConnectionConfig>> {
	const dotenv = await loadDotenvDefaults(context, workspaceFolder);
	return resolveSapConnectionDefaults(process.env, dotenv);
}

async function loadDotenvDefaults(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
): Promise<Map<string, string>> {
	for (const candidatePath of dotenvCandidatePaths(context, workspaceFolder)) {
		try {
			const content = await fs.promises.readFile(candidatePath, "utf8");
			return parseDotenvContents(content);
		} catch {
			continue;
		}
	}
	return new Map<string, string>();
}

function dotenvCandidatePaths(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
): string[] {
	const paths: string[] = [];
	const seen = new Set<string>();
	for (const basePath of [workspaceFolder.uri.fsPath, context.extensionPath, process.cwd()]) {
		for (const dir of ancestorDirectories(basePath)) {
			const candidate = path.join(dir, ".env");
			const normalized = path.normalize(candidate);
			if (seen.has(normalized)) {
				continue;
			}
			seen.add(normalized);
			paths.push(normalized);
		}
	}
	return paths;
}

function ancestorDirectories(startPath: string): string[] {
	const directories: string[] = [];
	let current = path.resolve(startPath);
	while (true) {
		directories.push(current);
		const parent = path.dirname(current);
		if (parent === current) {
			break;
		}
		current = parent;
	}
	return directories;
}

function normalizedNonEmpty(value: string | undefined): string | undefined {
	const trimmed = value?.trim();
	return trimmed ? trimmed : undefined;
}

export async function getSapConnectionConfig(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
	options: GetSapConnectionOptions = {},
): Promise<SapConnectionConfig | undefined> {
	const promptIfMissing = options.promptIfMissing ?? true;
	const config = vscode.workspace.getConfiguration("abap-ls", workspaceFolder.uri);
	const defaults = await loadSapConnectionDefaults(context, workspaceFolder);
	const storedBaseUrl = (config.get<string>("sap.baseUrl") ?? "").trim();
	const storedUsername = (config.get<string>("sap.username") ?? "").trim();
	const secretKey = secretKeyForWorkspace(workspaceFolder);
	const storedPassword = (await context.secrets.get(secretKey)) ?? "";

	let baseUrl = storedBaseUrl || defaults.baseUrl || "";
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

	let username = storedUsername || defaults.username || "";
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

	let password = storedPassword || defaults.password || "";
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
		case "message-class":
			return isMessageClassDependencyObject(objectRef);
		case "include":
			return loweredUri.includes("/programs/includes/") || loweredType === "PROG/I";
		case "report":
			return loweredUri.includes("/programs/programs/") || loweredType === "PROG/P";
		case "function":
			return loweredUri.includes("/functions/groups/") ||
				loweredType === "FUGR/F" ||
				loweredType === "FUGR/FF";
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
		isMessageClassDependencyObject(objectRef) ||
		isDdicDependencyObject(objectRef) ||
		loweredType === "PROG/I" ||
		loweredType === "PROG/P" ||
		loweredType.startsWith("CLAS/") ||
		loweredType.startsWith("INTF/");
}

export function isUnsupportedDomainDependencyObject(objectRef: AdtObjectRef): boolean {
	return objectRef.type.toUpperCase().startsWith("DOMA/");
}

export function hasOnlyUnsupportedExactDomainMatches(
	query: string,
	objects: AdtObjectRef[],
): boolean {
	const normalizedQuery = query.trim().toLowerCase();
	if (!normalizedQuery) {
		return false;
	}

	const exactMatches = objects.filter(
		(objectRef) => objectRef.name.trim().toLowerCase() === normalizedQuery,
	);
	return exactMatches.length > 0 &&
		exactMatches.every((objectRef) => isUnsupportedDomainDependencyObject(objectRef));
}

export function selectDependencyObjects(
	query: string,
	objects: AdtObjectRef[],
	kindHint?: string,
): AdtObjectRef[] {
	const normalizedQuery = query.trim().toLowerCase();
	if (!normalizedQuery) {
		return [];
	}

	const supportedExact = dedupeDependencyObjects(
		objects.filter((objectRef) =>
			objectRef.name.trim().toLowerCase() === normalizedQuery &&
			isSupportedDependencyObject(objectRef),
		),
	);
	if (supportedExact.length > 0) {
		return supportedExact;
	}

	const supportedByHint = objects.filter((objectRef) => isSupportedDependencyObject(objectRef, kindHint));
	const fallbackSupported = supportedByHint.length > 0
		? supportedByHint
		: objects.filter((objectRef) => isSupportedDependencyObject(objectRef));
	if (fallbackSupported.length === 0) {
		return [];
	}

	const preferred = pickBestDependencyObject(query, fallbackSupported, kindHint);
	return preferred ? [preferred] : [fallbackSupported[0]];
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
		return pickPreferredDependencyObject(exactMatches, kindHint) ?? exactMatches[0];
	}

	return pickPreferredDependencyObject(supported, kindHint) ?? supported[0];
}

function pickPreferredDependencyObject(
	objects: AdtObjectRef[],
	kindHint?: string,
): AdtObjectRef | undefined {
	if (objects.length === 0) {
		return undefined;
	}

	switch (kindHint?.trim().toLowerCase()) {
		case "report":
			return objects.find((objectRef) => objectRef.type.toUpperCase() === "PROG/P");
		case "function":
			return objects.find((objectRef) => objectRef.type.toUpperCase() === "FUGR/FF") ??
				objects.find((objectRef) => objectRef.type.toUpperCase() === "FUGR/F");
		case "static":
			return objects.find((objectRef) => objectRef.type.toUpperCase().startsWith("CLAS/")) ??
				objects.find((objectRef) => objectRef.type.toUpperCase().startsWith("INTF/"));
		case "type":
			return objects.find((objectRef) => isDdicDependencyObject(objectRef)) ??
				objects.find((objectRef) => objectRef.type.toUpperCase().startsWith("CLAS/")) ??
				objects.find((objectRef) => objectRef.type.toUpperCase().startsWith("INTF/"));
		default:
			return undefined;
	}
}

function dedupeDependencyObjects(objects: AdtObjectRef[]): AdtObjectRef[] {
	const deduped = new Map<string, AdtObjectRef>();
	for (const objectRef of objects) {
		const key = `${objectRef.type.toUpperCase()}::${objectRef.uri.toLowerCase()}`;
		if (!deduped.has(key)) {
			deduped.set(key, objectRef);
		}
	}
	return [...deduped.values()].sort((left, right) =>
		left.type.localeCompare(right.type) || left.uri.localeCompare(right.uri),
	);
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
	private static activeRequests = new Set<http.ClientRequest>();
	private csrfToken = "";
	private cookies: string[] = [];

	constructor(
		private readonly connection: SapConnectionConfig,
		private readonly options: AdtClientOptions = {},
	) {}

	static cancelAllActiveRequests(message = "ADT request cancelled."): void {
		const error = new AdtRequestCancelledError(message);
		for (const request of AdtClient.activeRequests) {
			request.destroy(error);
		}
	}

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

	async listFunctionGroupChildren(functionGroupName: string): Promise<AdtRepositoryChild[]> {
		const root = await this.fetchRepositoryNodeStructure(functionGroupName, "FUGR/F", []);
		const children: AdtRepositoryChild[] = [];

		if (root.objectTypes.length === 0) {
			return root.treeContent.map((node) => ({
				objectRef: repositoryNodeToObjectRef(node),
				categoryTag: "",
				objectTypeLabel: "",
				expandable: node.expandable,
			}));
		}

		for (const objectType of root.objectTypes) {
			if (!objectType.nodeId) {
				continue;
			}

			const branch = await this.fetchRepositoryNodeStructure(
				functionGroupName,
				"FUGR/F",
				[objectType.nodeId],
			);
			for (const node of branch.treeContent) {
				children.push({
					objectRef: repositoryNodeToObjectRef(node),
					categoryTag: objectType.categoryTag,
					objectTypeLabel: objectType.label,
					expandable: node.expandable,
				});
			}
		}

		return children;
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
		if (isMessageClassDependencyObject(objectRef)) {
			return {
				body: await this.fetchMessageClass(objectRef.name),
				fileExtension: "xml",
				manifestKind: "message-class",
			};
		}
		if (isDdicDependencyObject(objectRef)) {
			const ddicKind = inferDdicManifestKind(objectRef);
			const body = await this.fetchDdicObject(ddicKind, objectRef.name);
			return {
				body,
				fileExtension: "xml",
				manifestKind: ddicKind,
			};
		}
		if (isFunctionModuleObject(objectRef)) {
			return this.fetchFunctionModuleDependencySource(objectRef);
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

	async fetchMessageClass(name: string): Promise<string> {
		await this.ensureSession();
		const encodedName = encodeURIComponent(name);
		const response = await this.request(`/sap/bc/adt/messageclass/${encodedName}`, {
			headers: {
				Accept: "application/vnd.sap.adt.elementinfo+xml",
				"Cache-Control": "no-cache",
				"x-csrf-token": this.csrfToken,
			},
		});
		return formatDdicXml(response.body);
	}

	private async fetchFunctionModuleDependencySource(
		objectRef: AdtObjectRef,
	): Promise<AdtDependencyFetchResult> {
		const functionModuleSource = await this.fetchObjectSource(objectRef.uri);
		const functionGroupUri = inferFunctionGroupUri(objectRef);
		if (!functionGroupUri) {
			return {
				body: functionModuleSource,
				fileExtension: "abap",
				manifestKind: "function-module",
			};
		}

		let functionGroupSource: string;
		try {
			functionGroupSource = await this.fetchObjectSource(functionGroupUri);
		} catch {
			return {
				body: functionModuleSource,
				fileExtension: "abap",
				manifestKind: "function-module",
			};
		}

		// Function module ADT source lacks the surrounding function-pool context.
		// Keep the module in its own unit and fetch shared top-level includes separately.
		const sharedDependencies: AdtDependencyArtifact[] = [];
		await Promise.all(
			extractActiveTopLevelIncludeNames(functionGroupSource)
				.filter((includeName) => !isFunctionGroupDispatcherInclude(includeName))
				.map(async (includeName) => {
					try {
						const includeSource = await this.fetchObjectSource(
							`/sap/bc/adt/programs/includes/${encodeURIComponent(includeName)}`,
						);
						sharedDependencies.push({
							objectRef: buildIncludeObjectRef(includeName, objectRef.packageName),
							body: includeSource,
							fileExtension: "abap",
							manifestKind: "include",
						});
					} catch {
						// Keep the function module usable even when one shared include is unavailable.
					}
				}),
		);

		sharedDependencies.sort((left, right) => left.objectRef.name.localeCompare(right.objectRef.name));
		return {
			body: buildFunctionModuleDependencySource(functionGroupSource, functionModuleSource),
			fileExtension: "abap",
			manifestKind: "function-module",
			sharedDependencies,
		};
	}

	private async fetchRepositoryNodeStructure(
		parentName: string,
		parentType: string,
		nodeKeys: readonly string[],
	): Promise<RepositoryNodeStructure> {
		await this.ensureSession();

		const response = await this.request(
			`/sap/bc/adt/repository/nodestructure?parent_name=${encodeURIComponent(parentName)}&parent_tech_name=${encodeURIComponent(parentName)}&parent_type=${encodeURIComponent(parentType)}&withShortDescriptions=true`,
			{
				method: "POST",
				headers: {
					Accept: "application/vnd.sap.as+xml;charset=UTF-8;dataname=com.sap.adt.RepositoryObjectTreeContent",
					"Cache-Control": "no-cache",
					"Content-Type": "application/vnd.sap.as+xml; charset=UTF-8; dataname=null",
					"x-csrf-token": this.csrfToken,
				},
				body: buildNodeStructureRequestBody(nodeKeys),
			},
		);

		return parseRepositoryNodeStructure(response.body);
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
		this.throwIfCancelled();
		if (this.csrfToken) {
			return;
		}

		const response = await this.request("/sap/bc/adt/runtime/systemmessages", {
			headers: {
				Accept: SESSION_BOOTSTRAP_ACCEPT,
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

	private throwIfCancelled(): void {
		if (this.options.isCancelled?.()) {
			throw new AdtRequestCancelledError();
		}
	}

	private async request(
		pathOrUrl: string,
		options: {
			method?: string;
			headers?: Record<string, string>;
			body?: string;
		} = {},
	): Promise<HttpResponseData> {
		this.throwIfCancelled();
		await this.options.beforeRequest?.();
		this.throwIfCancelled();

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

			AdtClient.activeRequests.add(request);
			request.on("close", () => {
				AdtClient.activeRequests.delete(request);
			});
			request.on("error", reject);
			if (options.body !== undefined) {
				request.write(options.body, "utf8");
			}
			if (this.options.isCancelled?.()) {
				request.destroy(new AdtRequestCancelledError());
				return;
			}
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

export function isMessageClassDependencyObject(objectRef: AdtObjectRef): boolean {
	return objectRef.type.toUpperCase() === "MSAG/N" ||
		objectRef.uri.toLowerCase().includes("/sap/bc/adt/messageclass/");
}

export function buildMessageClassObjectRef(name: string): AdtObjectRef {
	const normalizedName = name.trim().toUpperCase();
	return {
		uri: `/sap/bc/adt/messageclass/${encodeURIComponent(normalizedName)}`,
		type: "MSAG/N",
		name: normalizedName,
		packageName: "",
		description: "Message class",
	};
}

function buildIncludeObjectRef(name: string, packageName: string): AdtObjectRef {
	const normalizedName = name.trim().toUpperCase();
	return {
		uri: `/sap/bc/adt/programs/includes/${encodeURIComponent(normalizedName)}`,
		type: "PROG/I",
		name: normalizedName,
		packageName,
		description: "Include",
	};
}

export function isFunctionModuleObject(objectRef: AdtObjectRef): boolean {
	return objectRef.type.toUpperCase() === "FUGR/FF" ||
		objectRef.uri.toLowerCase().includes("/functions/groups/") &&
		objectRef.uri.toLowerCase().includes("/fmodules/");
}

export function inferFunctionGroupUri(objectRef: AdtObjectRef): string | undefined {
	const match = objectRef.uri.match(/^(.*\/functions\/groups\/[^/]+)(?:\/fmodules\/[^/]+)?$/i);
	return match?.[1];
}

export function extractActiveTopLevelIncludeNames(source: string): string[] {
	const includeNames: string[] = [];
	const seen = new Set<string>();
	for (const rawLine of normalizeAbapSource(source).split("\n")) {
		const includeName = activeIncludeNameFromLine(rawLine);
		if (!includeName || seen.has(includeName)) {
			continue;
		}
		seen.add(includeName);
		includeNames.push(includeName);
	}
	return includeNames;
}

export function buildFunctionModuleDependencySource(
	functionGroupSource: string,
	functionModuleSource: string,
): string {
	const renderedGroup = normalizeAbapSource(functionGroupSource)
		.split("\n")
		.map((rawLine) => {
			const includeName = activeIncludeNameFromLine(rawLine);
			if (!includeName) {
				return rawLine;
			}

			if (isFunctionGroupDispatcherInclude(includeName)) {
				return `* INCLUDE ${includeName}. Omitted in dependency cache; function module stays in its own unit.`;
			}
			return rawLine;
		})
		.join("\n");

	return `${trimTrailingWhitespace(renderedGroup)}

${trimTrailingWhitespace(normalizeAbapSource(functionModuleSource))}
`;
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

function buildNodeStructureRequestBody(nodeKeys: readonly string[]): string {
	const values = nodeKeys.length > 0 ? nodeKeys : ["000000"];
	return `<?xml version="1.0" encoding="UTF-8" ?>
<asx:abap version="1.0" xmlns:asx="http://www.sap.com/abapxml">
<asx:values>
<DATA>
${values.map((value) => `<TV_NODEKEY>${escapeXmlText(value)}</TV_NODEKEY>`).join("\n")}
</DATA>
</asx:values>
</asx:abap>`;
}

function parseRepositoryNodeStructure(xml: string): RepositoryNodeStructure {
	const valuesBodyMatch = xml.match(/<asx:values\b[^>]*>([\s\S]*?)<\/asx:values>/i);
	const body = valuesBodyMatch?.[1] ?? xml;
	return {
		treeContent: collectBlocks(body, "SEU_ADT_REPOSITORY_OBJ_NODE").map((block) => ({
			objectType: readTagValue(block, "OBJECT_TYPE"),
			objectName: readTagValue(block, "OBJECT_NAME"),
			objectUri: readTagValue(block, "OBJECT_URI"),
			objectVitUri: readTagValue(block, "OBJECT_VIT_URI"),
			expandable: readTagValue(block, "EXPANDABLE").trim().toUpperCase() === "X",
		})).filter((entry) => entry.objectType && entry.objectName && entry.objectUri),
		objectTypes: collectBlocks(body, "SEU_ADT_OBJECT_TYPE_INFO").map((block) => ({
			objectType: readTagValue(block, "OBJECT_TYPE"),
			categoryTag: readTagValue(block, "CATEGORY_TAG"),
			label: readTagValue(block, "OBJECT_TYPE_LABEL"),
			nodeId: readTagValue(block, "NODE_ID"),
		})).filter((entry) => entry.objectType),
	};
}

function collectBlocks(xml: string, tagName: string): string[] {
	const matches = xml.match(new RegExp(`<${tagName}>([\\s\\S]*?)<\\/${tagName}>`, "gi")) ?? [];
	return matches;
}

function readTagValue(block: string, tagName: string): string {
	const match = block.match(new RegExp(`<${tagName}>([\\s\\S]*?)<\\/${tagName}>`, "i"));
	return decodeXmlEntity(match?.[1]?.trim() ?? "");
}

function repositoryNodeToObjectRef(node: RepositoryNodeEntry): AdtObjectRef {
	return {
		uri: node.objectUri,
		type: node.objectType,
		name: node.objectName,
		packageName: "",
		description: "",
	};
}

function escapeXmlText(value: string): string {
	return value
		.replace(/&/g, "&amp;")
		.replace(/</g, "&lt;")
		.replace(/>/g, "&gt;");
}

function activeIncludeNameFromLine(line: string): string | undefined {
	if (/^\s*\*/.test(line)) {
		return undefined;
	}

	const withoutTrailingComment = line.replace(/".*$/, "");
	const match = withoutTrailingComment.match(/^\s*include\s+([^\s.]+)\s*\.\s*$/i);
	return match?.[1]?.trim().toUpperCase();
}

function isFunctionGroupDispatcherInclude(includeName: string): boolean {
	return includeName.trim().toUpperCase().endsWith("UXX");
}

function normalizeAbapSource(source: string): string {
	return source.replace(/\r\n/g, "\n");
}

function trimTrailingWhitespace(source: string): string {
	return source.replace(/\s+$/u, "");
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

export function parseLocalDdicExportObjectRef(xml: string, fallbackName: string): AdtObjectRef | undefined {
	const trimmed = xml.trim();
	if (!trimmed.startsWith("<")) {
		return undefined;
	}

	const rootMatch = trimmed.match(/<abapsource:elementInfo\b([^>]*)>/i);
	if (!rootMatch) {
		return undefined;
	}

	const attributes = rootMatch[1] ?? "";
	const type = decodeXmlEntity(readAttribute(attributes, "adtcore:type")).trim().toUpperCase();
	if (!type) {
		return undefined;
	}

	const uri = decodeXmlEntity(readAttribute(attributes, "adtcore:uri")).trim();
	const rawName = decodeXmlEntity(readAttribute(attributes, "adtcore:name")).trim();
	const normalizedFallbackName = fallbackName.trim().toUpperCase();
	const name = (rawName || normalizedFallbackName).toUpperCase();
	if (!name) {
		return undefined;
	}

	const objectRef: AdtObjectRef = {
		uri,
		type,
		name,
		packageName: "",
		description: "",
	};
	return isDdicDependencyObject(objectRef) ? objectRef : undefined;
}

export function inferLocalExportObjectRef(
	source: string,
	fallbackName: string,
	kindHint = "",
): AdtObjectRef | undefined {
	const normalizedFallbackName = fallbackName.trim().toUpperCase();
	if (!normalizedFallbackName) {
		return undefined;
	}

	const trimmed = source.trim();
	if (trimmed.startsWith("<")) {
		const ddicObjectRef = parseLocalDdicExportObjectRef(trimmed, normalizedFallbackName);
		if (ddicObjectRef) {
			return ddicObjectRef;
		}
		return kindHint.trim().toLowerCase() === "message-class"
			? buildMessageClassObjectRef(normalizedFallbackName)
			: undefined;
	}

	for (const rawLine of normalizeAbapSource(source).split("\n")) {
		const line = rawLine.replace(/".*$/, "").trim();
		if (!line || line.startsWith("*")) {
			continue;
		}

		const classMatch = line.match(/^class\s+([^\s.]+)\b/i);
		if (classMatch?.[1]) {
			return buildLocalClassObjectRef(classMatch[1]);
		}

		const interfaceMatch = line.match(/^interface\s+([^\s.]+)\b/i);
		if (interfaceMatch?.[1]) {
			return buildLocalInterfaceObjectRef(interfaceMatch[1]);
		}

		const functionMatch = line.match(/^function\s+([^\s.]+)\b/i);
		if (functionMatch?.[1]) {
			return buildLocalFunctionModuleObjectRef(functionMatch[1]);
		}
	}

	switch (kindHint.trim().toLowerCase()) {
		case "include":
			return buildIncludeObjectRef(normalizedFallbackName, "");
		case "report":
			return buildLocalReportObjectRef(normalizedFallbackName);
		case "function":
			return buildLocalFunctionModuleObjectRef(normalizedFallbackName);
		case "static":
		case "symbol":
		case "type":
			return isLikelyInterfaceName(normalizedFallbackName)
				? buildLocalInterfaceObjectRef(normalizedFallbackName)
				: buildLocalClassObjectRef(normalizedFallbackName);
		default:
			return undefined;
	}
}

function buildLocalClassObjectRef(name: string): AdtObjectRef {
	const normalizedName = name.trim().toUpperCase();
	return {
		uri: `/sap/bc/adt/oo/classes/${encodeURIComponent(normalizedName)}`,
		type: "CLAS/OC",
		name: normalizedName,
		packageName: "",
		description: "Global class",
	};
}

function buildLocalInterfaceObjectRef(name: string): AdtObjectRef {
	const normalizedName = name.trim().toUpperCase();
	return {
		uri: `/sap/bc/adt/oo/interfaces/${encodeURIComponent(normalizedName)}`,
		type: "INTF/OI",
		name: normalizedName,
		packageName: "",
		description: "Global interface",
	};
}

function buildLocalFunctionModuleObjectRef(name: string): AdtObjectRef {
	const normalizedName = name.trim().toUpperCase();
	return {
		uri: `/sap/bc/adt/functions/groups/_local/fmodules/${encodeURIComponent(normalizedName)}`,
		type: "FUGR/FF",
		name: normalizedName,
		packageName: "",
		description: "Function module",
	};
}

function buildLocalReportObjectRef(name: string): AdtObjectRef {
	const normalizedName = name.trim().toUpperCase();
	return {
		uri: `/sap/bc/adt/programs/programs/${encodeURIComponent(normalizedName)}`,
		type: "PROG/P",
		name: normalizedName,
		packageName: "",
		description: "Report",
	};
}

function isLikelyInterfaceName(name: string): boolean {
	return /(?:^|\/)(?:[ZY][A-Z0-9]*_)?IF_/.test(name.trim().toUpperCase());
}
