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
export const dependencyCacheManifestDirName = "dependency-manifests";
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

export function dependencyCacheManifestPath(
	workspaceFolder: vscode.WorkspaceFolder,
	sourceFile: string,
): string {
	const normalizedSourceFile = normalizeRelativePath(sourceFile);
	return path.join(
		workspaceFolder.uri.fsPath,
		".abapls",
		"cache",
		dependencyCacheManifestDirName,
		`${encodeURIComponent(normalizedSourceFile)}.toml`,
	);
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

export function targetDependencyWorkspaceFilePath(
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
): string {
	const manifestUnit = inferManifestUnitSpec(
		objectRef,
		isXmlDependencyObject(objectRef) ? "dependency.xml" : "dependency.abap",
	);
	const kindDir = sanitizePathSegment(manifestUnit.kind);
	const packageDir = objectRef.packageName?.trim()
		? encodeWorkspaceObjectFileName(objectRef.packageName)
		: "_unknown";
	const fileExtension = isXmlDependencyObject(objectRef) ? "xml" : "abap";
	const fileName = `${encodeURIComponent(objectRef.name)}.${fileExtension}`;
	return path.join(
		workspaceFolder.uri.fsPath,
		".abapls",
		"cache",
		"packages",
		packageDir,
		kindDir,
		fileName,
	);
}

export async function ensureDependencyCacheUnit(
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
	filePath: string,
	sourceFiles: readonly string[],
): Promise<vscode.Uri> {
	const relativeFile = path.relative(workspaceFolder.uri.fsPath, filePath);
	const unit = inferManifestUnitSpec(objectRef, relativeFile);
	unit.dependencyOf = [];

	const normalizedSourceFiles = [...new Set(
		sourceFiles
			.map((file) => normalizeRelativePath(file))
			.filter((file) => file.length > 0),
	)];
	if (normalizedSourceFiles.length === 0) {
		throw new Error("ensureDependencyCacheUnit requires at least one source file");
	}

	let lastUri = vscode.Uri.file(dependencyCacheManifestPath(workspaceFolder, normalizedSourceFiles[0]));
	for (const sourceFile of normalizedSourceFiles) {
		const manifestPath = dependencyCacheManifestPath(workspaceFolder, sourceFile);
		await ensureTomlUnitFile(
			manifestPath,
			() => `${renderDependencyCacheManifestHeader(sourceFile)}\n`,
			unit,
		);
		lastUri = vscode.Uri.file(manifestPath);
	}
	return lastUri;
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
# Total ADT requests per second across all remote dependency fetches. Internal concurrency is derived from this rate.
remote_requests_per_second = ${defaultRemoteRequestsPerSecond}`;
}

function renderDependencyCacheManifestHeader(sourceFile: string): string {
	return `source_file = "${escapeTomlString(normalizeRelativePath(sourceFile))}"`;
}

function renderUnitBlock(unit: ManifestUnitSpec): string {
	const unitPackageNameLine = unit.packageName?.trim()
		? `package_name = "${escapeTomlString(unit.packageName)}"\n`
		: "";
	const members = normalizedManifestMembers(unit);
	const membersLine = renderMembersLine(members);
	const dependencyOf = normalizedManifestDependencyOf(unit);
	const dependencyOfLine = renderDependencyOfLine(dependencyOf);
	const trailingBlocks = [membersLine, dependencyOfLine]
		.filter((block) => block.length > 0)
		.join("\n\n");
	return `
[[unit]]
name = "${escapeTomlString(unit.name)}"
kind = "${escapeTomlString(unit.kind)}"
root_file = "${escapeTomlString(normalizeRelativePath(unit.rootFile))}"
${unitPackageNameLine}
${trailingBlocks ? `\n${trailingBlocks}` : ""}
`;
}

function escapeTomlString(value: string): string {
	return value.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}

function normalizeRelativePath(value: string): string {
	return value.replace(/\\/g, "/").replace(/^\.\//, "");
}

function findManifestUnit(text: string, unit: ManifestUnitSpec): ManifestUnitMatch | undefined {
	const candidateName = normalizeCandidateName(unit.name);
	const candidateRootFile = normalizeCandidateFile(unit.rootFile);
	const matches = [...text.matchAll(/^\[\[unit\]\]\s*$/gm)];
	for (let index = 0; index < matches.length; index += 1) {
		const start = matches[index].index ?? 0;
		const end = matches[index + 1]?.index ?? text.length;
		const block = text.slice(start, end);
		const blockName = normalizeCandidateName(readTomlString(block, "name"));
		const blockRootFile = normalizeCandidateFile(readTomlString(block, "root_file"));
		if (
			(candidateRootFile && blockRootFile === candidateRootFile) ||
			(candidateName && blockName === candidateName)
		) {
			return { start, end };
		}
	}
	return undefined;
}

function mergeManifestUnit(block: string, unit: ManifestUnitSpec): ManifestUnitSpec {
	if (!unit.dependencyOf?.length) {
		return unit;
	}

	return {
		...unit,
		dependencyOf: [
			...readManifestDependencyOf(block),
			...unit.dependencyOf,
		],
	};
}

function readManifestDependencyOf(block: string): ManifestUnitDependencyOfSpec[] {
	return inlineDependencyOf(block).map((file) => ({ file }));
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

function normalizedManifestMembers(unit: ManifestUnitSpec): ManifestUnitMemberSpec[] {
	if (unit.members?.length) {
		return unit.members.map((member) => ({
			file: normalizeRelativePath(member.file),
			objectName: member.objectName,
		})).filter((member) => member.file !== normalizeRelativePath(unit.rootFile));
	}

	return [];
}

function manifestMemberObjectNameIsDefault(member: ManifestUnitMemberSpec): boolean {
	const explicit = member.objectName?.trim();
	if (!explicit) {
		return true;
	}
	const inferred = inferManifestMemberObjectName(member.file);
	return inferred !== undefined && explicit.toUpperCase() === inferred.toUpperCase();
}

function renderMembersLine(members: ManifestUnitMemberSpec[]): string {
	if (members.length === 0) {
		return "";
	}
	const renderedMembers = members.map((member) => renderInlineManifestMember(member)).join(",\n");
	return `members = [\n${indentTomlBlock(renderedMembers, 1)}\n]`;
}

function renderDependencyOfLine(dependencies: ManifestUnitDependencyOfSpec[]): string {
	if (dependencies.length === 0) {
		return "";
	}
	const rendered = dependencies
		.map((dependency) => `"${escapeTomlString(normalizeRelativePath(dependency.file))}"`)
		.join(",\n");
	return `dependency_of = [\n${indentTomlBlock(rendered, 1)}\n]`;
}

function renderInlineManifestMember(member: ManifestUnitMemberSpec): string {
	const normalizedFile = normalizeRelativePath(member.file);
	if (manifestMemberObjectNameIsDefault(member)) {
		return `"${escapeTomlString(normalizedFile)}"`;
	}

	const properties = [
		`file = "${escapeTomlString(normalizedFile)}"`,
		!manifestMemberObjectNameIsDefault(member)
			? `object_name = "${escapeTomlString(member.objectName ?? "")}"`
			: undefined,
	].filter((value): value is string => Boolean(value));
	return `{ ${properties.join(", ")} }`;
}

function indentTomlBlock(value: string, level: number): string {
	const indent = "\t".repeat(level);
	return value
		.split("\n")
		.map((line) => `${indent}${line}`)
		.join("\n");
}

function inlineDependencyOf(block: string): string[] {
	const match = block.match(/^dependency_of\s*=\s*\[([\s\S]*?)^\]\s*$/m);
	if (!match) {
		return [];
	}
	const body = match[1];
	const dependencies: string[] = [];
	for (const stringMatch of body.matchAll(/"((?:[^"\\]|\\.)*)"/g)) {
		dependencies.push(stringMatch[1].replace(/\\"/g, "\"").replace(/\\\\/g, "\\"));
	}
	return dependencies.map((value) => normalizeRelativePath(value));
}

function normalizeCandidateName(value: string | undefined): string | undefined {
	const trimmed = value?.trim();
	return trimmed ? trimmed.toUpperCase() : undefined;
}

function normalizeCandidateFile(value: string | undefined): string | undefined {
	const trimmed = value?.trim();
	return trimmed ? normalizeRelativePath(trimmed) : undefined;
}

function inferManifestMemberObjectName(file: string): string | undefined {
	const normalizedFile = normalizeRelativePath(file);
	const baseName = path.posix.basename(normalizedFile);
	if (!baseName) {
		return undefined;
	}
	const suffixIndex = baseName.lastIndexOf(".");
	const stem = suffixIndex >= 0 ? baseName.slice(0, suffixIndex) : baseName;
	if (!stem) {
		return undefined;
	}
	return decodeURIComponent(stem).trim().toUpperCase();
}

function normalizedManifestDependencyOf(unit: ManifestUnitSpec): ManifestUnitDependencyOfSpec[] {
	const deduped = new Set<string>();
	const dependencies = unit.dependencyOf ?? [];
	return dependencies
		.map((dependency) => ({
			file: normalizeRelativePath(dependency.file),
		}))
		.filter((dependency) => {
			if (!dependency.file || deduped.has(dependency.file)) {
				return false;
			}
			deduped.add(dependency.file);
			return true;
		});
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

async function ensureTomlUnitFile(
	filePath: string,
	initialText: () => string,
	unit: ManifestUnitSpec,
): Promise<void> {
	await withManifestUpdateLock(filePath, async () => {
		await fs.promises.mkdir(path.dirname(filePath), { recursive: true });
		const existing = await readTextIfExists(filePath);

		if (!existing) {
			const unitBlock = renderUnitBlock(unit);
			await fs.promises.writeFile(filePath, `${initialText()}${unitBlock}`, "utf8");
			return;
		}

		const match = findManifestUnit(existing, unit);
		if (match) {
			const mergedUnit = mergeManifestUnit(existing.slice(match.start, match.end), unit);
			const unitBlock = renderUnitBlock(mergedUnit);
			const updated = `${existing.slice(0, match.start)}${unitBlock}${existing.slice(match.end)}`;
			await fs.promises.writeFile(filePath, updated, "utf8");
			return;
		}

		const unitBlock = renderUnitBlock(unit);
		const separator = existing.endsWith("\n") ? "\n" : "\n\n";
		await fs.promises.writeFile(filePath, `${existing}${separator}${unitBlock}`, "utf8");
	});
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
