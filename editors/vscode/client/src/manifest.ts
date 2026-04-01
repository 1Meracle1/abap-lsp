import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import type { AdtObjectRef } from "./adt";

export interface ManifestUnitSpec {
	name: string;
	kind: string;
	rootFile: string;
	adtUri: string;
	role: string;
	objectName: string;
}

export type ManifestDependencyMode = "remote-on-demand" | "local-first";
export type ManifestUnknownSymbolMode = "remote" | "log";

export interface ManifestOptions {
	dependencyMode?: ManifestDependencyMode;
	unknownSymbolMode?: ManifestUnknownSymbolMode;
}

export const manifestFileName = "abapls.toml";
export const unknownSymbolLogPath = ".abapls/logs/unknown-symbols.log";
export const defaultRemoteRequestParallelism = 4;
export const defaultRemoteRequestsPerSecond = 8;
export const dependencyModeRemoteOnDemand: ManifestDependencyMode = "remote-on-demand";
export const dependencyModeLocalFirst: ManifestDependencyMode = "local-first";
export const unknownSymbolModeRemote: ManifestUnknownSymbolMode = "remote";
export const unknownSymbolModeLog: ManifestUnknownSymbolMode = "log";

export function inferManifestUnitSpec(objectRef: AdtObjectRef, relativeFilePath: string): ManifestUnitSpec {
	const normalizedFile = normalizeRelativePath(relativeFilePath);
	const loweredUri = objectRef.uri.toLowerCase();
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
	const existing = await readTextIfExists(manifestPath);
	const unitBlock = renderUnitBlock(unit);

	if (!existing) {
		const initialText = `${renderManifestHeader(options)}\n${unitBlock}`;
		await fs.promises.writeFile(manifestPath, initialText, "utf8");
		return vscode.Uri.file(manifestPath);
	}

	if (existing.includes(`adt_uri = "${unit.adtUri}"`) || existing.includes(`name = "${unit.name}"`)) {
		return vscode.Uri.file(manifestPath);
	}

	const separator = existing.endsWith("\n") ? "\n" : "\n\n";
	await fs.promises.writeFile(manifestPath, `${existing}${separator}${unitBlock}`, "utf8");
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
	const existing = await readTextIfExists(manifestPath);
	if (existing !== undefined) {
		return vscode.Uri.file(manifestPath);
	}

	await fs.promises.writeFile(manifestPath, `${renderManifestHeader(options)}\n`, "utf8");
	return vscode.Uri.file(manifestPath);
}

export function targetWorkspaceFilePath(workspaceFolder: vscode.WorkspaceFolder, objectName: string): string {
	return path.join(workspaceFolder.uri.fsPath, "src", `${objectName}.abap`);
}

export function targetDependencyWorkspaceFilePath(
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
): string {
	const kindDir = sanitizePathSegment(inferManifestUnitSpec(objectRef, "dependency.abap").kind);
	const fileName = `${encodeURIComponent(objectRef.name)}.abap`;
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
	return `
[[unit]]
name = "${escapeTomlString(unit.name)}"
kind = "${escapeTomlString(unit.kind)}"
root_file = "${escapeTomlString(normalizeRelativePath(unit.rootFile))}"
adt_uri = "${escapeTomlString(unit.adtUri)}"

[[unit.member]]
role = "${escapeTomlString(unit.role)}"
file = "${escapeTomlString(normalizeRelativePath(unit.rootFile))}"
object_name = "${escapeTomlString(unit.objectName)}"
adt_uri = "${escapeTomlString(unit.adtUri)}"`;
}

function escapeTomlString(value: string): string {
	return value.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}

function normalizeRelativePath(value: string): string {
	return value.replace(/\\/g, "/").replace(/^\.\//, "");
}

function sanitizePathSegment(value: string): string {
	return value.replace(/[^a-zA-Z0-9._-]+/g, "-");
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
