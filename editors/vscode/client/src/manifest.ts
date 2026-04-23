import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
	inferDdicManifestKind,
	isDdicDependencyObject,
	isFunctionModuleObject,
	isMessageClassDependencyObject,
	type AdtObjectRef,
} from "./adt";

export interface ManifestUnitSpec {
	name: string;
	kind: string;
	rootFile: string;
	packageName?: string;
	dependencyOf?: ManifestUnitDependencyOfSpec[];
	members?: ManifestUnitMemberSpec[];
}

export interface ManifestUnitDependencyOfSpec {
	file: string;
}

export interface ManifestUnitMemberSpec {
	file: string;
	objectName?: string;
}

const pendingManifestUpdates = new Map<string, Promise<void>>();

export type ManifestDependencyMode = "remote-on-demand";

export interface ManifestOptions {
	dependencyMode?: ManifestDependencyMode;
}

export const manifestFileName = "abapls.toml";
export const defaultRemoteRequestParallelism = 8;
export const defaultRemoteRequestsPerSecond = 24;
export const dependencyModeRemoteOnDemand: ManifestDependencyMode = "remote-on-demand";

export function inferManifestUnitSpec(objectRef: AdtObjectRef, relativeFilePath: string): ManifestUnitSpec {
	const normalizedFile = normalizeRelativePath(relativeFilePath);
	const loweredUri = objectRef.uri.toLowerCase();
	if (isDdicDependencyObject(objectRef)) {
		return {
			name: objectRef.name,
			kind: inferDdicManifestKind(objectRef),
			rootFile: normalizedFile,
			packageName: objectRef.packageName,
		};
	}
	if (isMessageClassDependencyObject(objectRef)) {
		return {
			name: objectRef.name,
			kind: "message-class",
			rootFile: normalizedFile,
			packageName: objectRef.packageName,
		};
	}
	if (loweredUri.includes("/programs/includes/") || objectRef.type === "PROG/I") {
		return {
			name: objectRef.name,
			kind: "include",
			rootFile: normalizedFile,
			packageName: objectRef.packageName,
		};
	}
	if (isFunctionModuleObject(objectRef)) {
		return {
			name: objectRef.name,
			kind: "function-module",
			rootFile: normalizedFile,
			packageName: objectRef.packageName,
		};
	}
	if (loweredUri.includes("/oo/classes/") || objectRef.type.startsWith("CLAS/")) {
		return {
			name: objectRef.name,
			kind: "global-class",
			rootFile: normalizedFile,
			packageName: objectRef.packageName,
		};
	}
	if (loweredUri.includes("/oo/interfaces/") || objectRef.type.startsWith("INTF/")) {
		return {
			name: objectRef.name,
			kind: "global-interface",
			rootFile: normalizedFile,
			packageName: objectRef.packageName,
		};
	}
	if (loweredUri.includes("/functions/groups/")) {
		return {
			name: objectRef.name,
			kind: "function-group",
			rootFile: normalizedFile,
			packageName: objectRef.packageName,
		};
	}
	return {
		name: objectRef.name,
		kind: "report",
		rootFile: normalizedFile,
		packageName: objectRef.packageName,
	};
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

export function targetLocalWorkspaceFilePath(
	workspaceFolder: vscode.WorkspaceFolder,
	kind: string,
	objectName: string,
): string {
	const encodedName = `${encodeWorkspaceObjectFileName(objectName)}.abap`;
	const normalizedKind = kind.trim().toLowerCase();
	const kindDirs = localWorkspaceKindDirectories(normalizedKind, objectName);
	return path.join(
		workspaceFolder.uri.fsPath,
		"src",
		...kindDirs,
		encodedName,
	);
}

export function targetEditableWorkspaceFilePath(
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
): string {
	const inferred = inferManifestUnitSpec(objectRef, "placeholder.abap");
	return targetLocalWorkspaceFilePath(workspaceFolder, inferred.kind, objectRef.name);
}

function renderManifestHeader(options: ManifestOptions = {}): string {
	const dependencyMode = options.dependencyMode ?? dependencyModeRemoteOnDemand;
	return `version = 1
connection = "default"

[resolution]
dependency_mode = "${dependencyMode}"
# Total ADT requests per second across all remote dependency fetches. Internal concurrency is derived from this rate.
remote_requests_per_second = ${defaultRemoteRequestsPerSecond}

# Configure this block to enable centralized remote dependency storage and SAP-versioned lookups.
# [dependency_store]
# product_version = "SAP NETWEAVER"
# default_package_version = "7.50"`;
}

export function manifestUsesCentralDependencyStore(text: string): boolean {
	const block = readManifestSection(text, "dependency_store");
	if (block === undefined) {
		return false;
	}
	const productVersion = readTomlString(block, "product_version")?.trim();
	const defaultPackageVersion = readTomlString(block, "default_package_version")?.trim();
	return Boolean(productVersion && defaultPackageVersion);
}

function readManifestSection(text: string, sectionName: string): string | undefined {
	const normalizedSectionName = sectionName.trim().toLowerCase();
	const lines = text.split(/\r?\n/);
	let currentSection: string | undefined;
	const body: string[] = [];

	for (const line of lines) {
		const sectionMatch = line.match(/^\s*\[([^\]]+)\]\s*$/);
		if (sectionMatch) {
			if (currentSection === normalizedSectionName) {
				break;
			}
			currentSection = sectionMatch[1].trim().toLowerCase();
			continue;
		}
		if (currentSection === normalizedSectionName) {
			body.push(line);
		}
	}

	if (currentSection !== normalizedSectionName && body.length === 0) {
		return undefined;
	}
	return body.join("\n");
}

function normalizeRelativePath(value: string): string {
	return value.replace(/\\/g, "/").replace(/^\.\//, "");
}

function readTomlString(block: string, key: string): string | undefined {
	const escapedKey = key.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	const match = block.match(new RegExp(`^${escapedKey}\\s*=\\s*"(.*)"\\s*$`, "m"));
	if (!match) {
		return undefined;
	}
	return match[1].replace(/\\"/g, "\"").replace(/\\\\/g, "\\");
}

function encodeWorkspaceObjectFileName(objectName: string): string {
	return encodeURIComponent(objectName.trim().toUpperCase());
}

function localWorkspaceKindDirectories(kind: string, objectName: string): string[] {
	const encodedObjectName = encodeWorkspaceObjectFileName(objectName);
	switch (kind) {
		case "global-class":
			return ["classes"];
		case "global-interface":
			return ["interfaces"];
		case "include":
			return ["includes"];
		case "report":
			return ["reports", encodedObjectName];
		default:
			return ["misc"];
	}
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
