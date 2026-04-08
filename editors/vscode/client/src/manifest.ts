import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
	inferDdicManifestKind,
	isDdicDependencyObject,
	isMessageClassDependencyObject,
	type AdtObjectRef,
} from "./adt";

export interface ManifestUnitSpec {
	name: string;
	kind: string;
	rootFile: string;
	adtUri?: string;
	role: string;
	objectName: string;
}

interface ManifestUnitMatch {
	start: number;
	end: number;
}

const pendingManifestUpdates = new Map<string, Promise<void>>();

export type ManifestDependencyMode = "remote-on-demand" | "local-first";
export type ManifestUnknownSymbolMode = "remote" | "log";

export interface ManifestOptions {
	dependencyMode?: ManifestDependencyMode;
	unknownSymbolMode?: ManifestUnknownSymbolMode;
}

export const manifestFileName = "abapls.toml";
export const unknownSymbolLogPath = ".abapls/logs/unknown-symbols.log";
export const defaultRemoteRequestParallelism = 8;
export const defaultRemoteRequestsPerSecond = 24;
export const dependencyModeRemoteOnDemand: ManifestDependencyMode = "remote-on-demand";
export const dependencyModeLocalFirst: ManifestDependencyMode = "local-first";
export const unknownSymbolModeRemote: ManifestUnknownSymbolMode = "remote";
export const unknownSymbolModeLog: ManifestUnknownSymbolMode = "log";

export function inferManifestUnitSpec(objectRef: AdtObjectRef, relativeFilePath: string): ManifestUnitSpec {
	const normalizedFile = normalizeRelativePath(relativeFilePath);
	const loweredUri = objectRef.uri.toLowerCase();
	if (isDdicDependencyObject(objectRef)) {
		return {
			name: objectRef.name,
			kind: inferDdicManifestKind(objectRef),
			rootFile: normalizedFile,
			adtUri: objectRef.uri,
			role: "dependency",
			objectName: objectRef.name,
		};
	}
	if (isMessageClassDependencyObject(objectRef)) {
		return {
			name: objectRef.name,
			kind: "message-class",
			rootFile: normalizedFile,
			adtUri: objectRef.uri,
			role: "dependency",
			objectName: objectRef.name,
		};
	}
	if (loweredUri.includes("/programs/includes/") || objectRef.type === "PROG/I") {
		return {
			name: objectRef.name,
			kind: "include",
			rootFile: normalizedFile,
			adtUri: objectRef.uri,
			role: "root",
			objectName: objectRef.name,
		};
	}
	if (loweredUri.includes("/oo/classes/") || objectRef.type.startsWith("CLAS/")) {
		return {
			name: objectRef.name,
			kind: "global-class",
			rootFile: normalizedFile,
			adtUri: objectRef.uri,
			role: "main",
			objectName: objectRef.name,
		};
	}
	if (loweredUri.includes("/oo/interfaces/") || objectRef.type.startsWith("INTF/")) {
		return {
			name: objectRef.name,
			kind: "global-interface",
			rootFile: normalizedFile,
			adtUri: objectRef.uri,
			role: "main",
			objectName: objectRef.name,
		};
	}
	if (loweredUri.includes("/functions/groups/")) {
		return {
			name: objectRef.name,
			kind: "function-group",
			rootFile: normalizedFile,
			adtUri: objectRef.uri,
			role: "main",
			objectName: objectRef.name,
		};
	}
	return {
		name: objectRef.name,
		kind: "report",
		rootFile: normalizedFile,
		adtUri: objectRef.uri,
		role: "root",
		objectName: objectRef.name,
	};
}

export async function ensureManifestUnit(
	workspaceFolder: vscode.WorkspaceFolder,
	unit: ManifestUnitSpec,
	options: ManifestOptions = {},
): Promise<vscode.Uri> {
	const manifestPath = workspaceManifestPath(workspaceFolder);
	await withManifestUpdateLock(manifestPath, async () => {
		const existing = await readTextIfExists(manifestPath);
		const unitBlock = renderUnitBlock(unit);

		if (!existing) {
			const initialText = `${renderManifestHeader(options)}\n${unitBlock}`;
			await fs.promises.writeFile(manifestPath, initialText, "utf8");
			return;
		}

		const match = findManifestUnit(existing, unit);
		if (match) {
			const updated = `${existing.slice(0, match.start)}${unitBlock}${existing.slice(match.end)}`;
			await fs.promises.writeFile(manifestPath, updated, "utf8");
			return;
		}

		const separator = existing.endsWith("\n") ? "\n" : "\n\n";
		await fs.promises.writeFile(manifestPath, `${existing}${separator}${unitBlock}`, "utf8");
	});
	return vscode.Uri.file(manifestPath);
}

export function workspaceManifestPath(workspaceFolder: vscode.WorkspaceFolder): string {
	return path.join(workspaceFolder.uri.fsPath, manifestFileName);
}

export async function ensureWorkspaceManifest(
	workspaceFolder: vscode.WorkspaceFolder,
	options: ManifestOptions = {},
): Promise<vscode.Uri> {
	const manifestPath = workspaceManifestPath(workspaceFolder);
	await withManifestUpdateLock(manifestPath, async () => {
		const existing = await readTextIfExists(manifestPath);
		if (existing !== undefined) {
			return;
		}

		await fs.promises.writeFile(manifestPath, `${renderManifestHeader(options)}\n`, "utf8");
	});
	return vscode.Uri.file(manifestPath);
}

export function targetWorkspaceFilePath(workspaceFolder: vscode.WorkspaceFolder, objectName: string): string {
	return path.join(workspaceFolder.uri.fsPath, "src", `${objectName}.abap`);
}

export function targetDependencyWorkspaceFilePath(
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
): string {
	const manifestUnit = inferManifestUnitSpec(
		objectRef,
		isXmlDependencyObject(objectRef) ? "dependency.xml" : "dependency.abap",
	);
	const kindDir = sanitizePathSegment(manifestUnit.kind);
	const fileExtension = isXmlDependencyObject(objectRef) ? "xml" : "abap";
	const fileName = `${encodeURIComponent(objectRef.name)}.${fileExtension}`;
	return path.join(workspaceFolder.uri.fsPath, ".abapls", "cache", "dependencies", kindDir, fileName);
}

export async function ensureManifestDependencyUnit(
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
	filePath: string,
): Promise<vscode.Uri> {
	const relativeFile = path.relative(workspaceFolder.uri.fsPath, filePath);
	const unit = inferManifestUnitSpec(objectRef, relativeFile);
	unit.role = "dependency";
	return ensureManifestUnit(workspaceFolder, unit);
}

function renderManifestHeader(options: ManifestOptions = {}): string {
	const dependencyMode = options.dependencyMode ?? dependencyModeRemoteOnDemand;
	const unknownSymbolMode = options.unknownSymbolMode ??
		(dependencyMode === dependencyModeLocalFirst ? unknownSymbolModeLog : unknownSymbolModeRemote);
	return `version = 1
connection = "default"

[resolution]
# "local-first" keeps dependency resolution inside the workspace; "remote-on-demand" enables ADT fetches.
dependency_mode = "${dependencyMode}"
cache_dir = ".abapls/cache"
# "remote" performs ADT fetches when remote dependency resolution is enabled; "log" writes unknown symbol candidates to ${unknownSymbolLogPath}
unknown_symbol_mode = "${unknownSymbolMode}"
# Maximum number of remote dependency fetches in flight at once when dependency_mode = "remote-on-demand".
remote_request_parallelism = ${defaultRemoteRequestParallelism}
# Total ADT requests per second across all remote dependency fetches.
remote_requests_per_second = ${defaultRemoteRequestsPerSecond}`;
}

function renderUnitBlock(unit: ManifestUnitSpec): string {
	const unitAdtUriLine = unit.adtUri?.trim()
		? `adt_uri = "${escapeTomlString(unit.adtUri)}"\n`
		: "";
	const memberAdtUriLine = unit.adtUri?.trim()
		? `adt_uri = "${escapeTomlString(unit.adtUri)}"\n`
		: "";
	return `
[[unit]]
name = "${escapeTomlString(unit.name)}"
kind = "${escapeTomlString(unit.kind)}"
root_file = "${escapeTomlString(normalizeRelativePath(unit.rootFile))}"
${unitAdtUriLine}

[[unit.member]]
role = "${escapeTomlString(unit.role)}"
file = "${escapeTomlString(normalizeRelativePath(unit.rootFile))}"
object_name = "${escapeTomlString(unit.objectName)}"
${memberAdtUriLine}
`;
}

function escapeTomlString(value: string): string {
	return value.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}

function normalizeRelativePath(value: string): string {
	return value.replace(/\\/g, "/").replace(/^\.\//, "");
}

function findManifestUnit(text: string, unit: ManifestUnitSpec): ManifestUnitMatch | undefined {
	const matches = [...text.matchAll(/^\[\[unit\]\]\s*$/gm)];
	for (let index = 0; index < matches.length; index += 1) {
		const start = matches[index].index ?? 0;
		const end = matches[index + 1]?.index ?? text.length;
		const block = text.slice(start, end);
		const adtUri = readTomlString(block, "adt_uri");
		const name = readTomlString(block, "name");
		const rootFile = readTomlString(block, "root_file");
		const normalizedUnitAdtUri = unit.adtUri?.trim();
		if (normalizedUnitAdtUri && adtUri === normalizedUnitAdtUri) {
			return { start, end };
		}
		if (name === unit.name || rootFile === normalizeRelativePath(unit.rootFile)) {
			return { start, end };
		}
	}
	return undefined;
}

function readTomlString(block: string, key: string): string | undefined {
	const escapedKey = key.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	const match = block.match(new RegExp(`^${escapedKey}\\s*=\\s*"(.*)"\\s*$`, "m"));
	if (!match) {
		return undefined;
	}
	return match[1].replace(/\\"/g, "\"").replace(/\\\\/g, "\\");
}

function sanitizePathSegment(value: string): string {
	return value.replace(/[^a-zA-Z0-9._-]+/g, "-");
}

function isXmlDependencyObject(objectRef: AdtObjectRef): boolean {
	return isDdicDependencyObject(objectRef) || isMessageClassDependencyObject(objectRef);
}

async function readTextIfExists(filePath: string): Promise<string | undefined> {
	try {
		return await fs.promises.readFile(filePath, "utf8");
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === "ENOENT") {
			return undefined;
		}
		throw error;
	}
}

async function withManifestUpdateLock<T>(manifestPath: string, action: () => Promise<T>): Promise<T> {
	const previous = pendingManifestUpdates.get(manifestPath) ?? Promise.resolve();
	let release!: () => void;
	const current = new Promise<void>((resolve) => {
		release = resolve;
	});
	pendingManifestUpdates.set(manifestPath, previous.then(() => current));

	await previous;
	try {
		return await action();
	} finally {
		release();
		if (pendingManifestUpdates.get(manifestPath) === current) {
			pendingManifestUpdates.delete(manifestPath);
		}
	}
}
