/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as fs from "fs";
import * as net from "net";
import * as path from "path";
import * as vscode from "vscode";

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	StreamInfo,
	TransportKind,
} from "vscode-languageclient/node";
import {
	AdtClient,
	AdtRepositoryChild,
	AdtObjectRef,
	buildMessageClassObjectRef,
	configureSapConnection,
	inferFunctionGroupUri,
	isFunctionModuleObject,
	getSapConnectionConfig,
	hasOnlyUnsupportedExactDomainMatches,
	pickBestDependencyObject,
} from "./adt";
import {
	dependencyModeLocalFirst,
	ensureManifestDependencyUnit,
	ensureWorkspaceManifest,
	ensureManifestUnit,
	inferManifestUnitSpec,
	manifestFileName,
	type ManifestUnitMemberSpec,
	type ManifestUnitSpec,
	targetDependencyWorkspaceFilePath,
	targetWorkspaceFilePath,
	unknownSymbolModeLog,
	unknownSymbolLogPath,
	unknownSymbolModeRemote,
	workspaceManifestPath,
} from "./manifest";
import {
	hasCachedRemoteDependencyCandidate,
	dedupeRemoteDependencyCandidates,
	hasNegativeRemoteDependencyCandidate,
	markNegativeRemoteDependencyCandidate,
	RemoteDependencyCandidate,
	RemoteDependencyFetchPolicy,
	RemoteDependencyScheduler,
} from "./remoteDependencies";

let client: LanguageClient;
const pendingRemoteDependencyFetches = new Map<string, Promise<string | undefined>>();
const negativeRemoteDependencyCache = new Set<string>();
const remoteDependencySchedulers = new Map<string, RemoteDependencyScheduler>();
const pendingWorkspaceConfigPrompts = new Set<string>();
const dismissedWorkspaceConfigPrompts = new Set<string>();
const workspaceAnalysisProgress = new Map<string, WorkspaceAnalysisProgressHandle>();
const customerObjectNamePattern = /^(?:[ZY][A-Z0-9_\/]*|\/[A-Z0-9_]+\/[A-Z0-9_\/]+)$/;

interface RemoteDependencyResolveParams {
	workspaceUri: string;
	sourceUri: string;
	sourceUris?: string[];
	unknownSymbolMode?: string;
	remoteRequestParallelism?: number;
	remoteRequestsPerSecond?: number;
	candidates: RemoteDependencyCandidate[];
}

interface RemoteDependenciesUpdatedParams {
	workspaceUri: string;
	sourceUri: string;
	sourceUris?: string[];
	fetched: string[];
	failed: RemoteDependencyCandidate[];
}

interface RemoteDependencyResolutionResult {
	candidate: RemoteDependencyCandidate;
	fetchedName?: string;
}

interface WorkspaceManifestUpdatedParams {
	workspaceUri: string;
}

type WorkspaceAnalysisPhase = "started" | "progress" | "finished";

interface WorkspaceAnalysisStatusParams {
	workspaceUri: string;
	phase: WorkspaceAnalysisPhase;
	trigger: string;
	processedDocumentCount: number;
	totalDocumentCount: number;
	analyzedDocumentCount: number;
	remoteResolutionInFlight: boolean;
}

interface WorkspaceAnalysisProgressHandle {
	resolve?: () => void;
	report?: (params: WorkspaceAnalysisStatusParams) => void;
	showTimer?: NodeJS.Timeout;
	latest?: WorkspaceAnalysisStatusParams;
}

interface LocalWorkspaceObjectTemplate {
	label: string;
	kind: string;
	role: string;
	namePattern: RegExp;
	namePlaceholder: string;
	stub: (name: string) => string;
}

interface AbapFoldingRangeShape {
	start: number;
	end: number;
	kind?: vscode.FoldingRangeKind;
}

interface BlockEntry {
	kind: "simple" | "if" | "case";
	startLine: number;
	endKeyword?: string;
	currentArmStartLine?: number;
}

export function activate(context: vscode.ExtensionContext) {
	const serverOptions = buildServerOptions();
	const clientDocumentSelector = [
		{ scheme: "file", language: "abap" },
		{ scheme: "untitled", language: "abap" },
	];
	const foldingDocumentSelector: vscode.DocumentFilter[] = [
		{ scheme: "file", language: "abap" },
		{ scheme: "untitled", language: "abap" },
	];

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: clientDocumentSelector,
		synchronize: {
			fileEvents: [
				vscode.workspace.createFileSystemWatcher("**/.clientrc"),
				vscode.workspace.createFileSystemWatcher("**/abapls.toml"),
				vscode.workspace.createFileSystemWatcher("**/.abapls/cache/**/*.abap"),
			],
		},
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		"abap-ls",
		"ABAP Language Server",
		serverOptions,
		clientOptions,
	);

	context.subscriptions.push(
		vscode.languages.registerFoldingRangeProvider(foldingDocumentSelector, {
			provideFoldingRanges(document) {
				return computeAbapFoldingRanges(document.getText()).map(
					(range) => new vscode.FoldingRange(range.start, range.end, range.kind),
				);
			},
		}),
	);

	registerCommands(context);
	registerClientNotifications(context);

	// Start the client. This will also launch the server
	client.start();
	registerWorkspaceConfigPrompts(context);
}

/**
 * Production: `serverTransport` "stdio" — extension spawns `serverExecutable` (or __ABAP_LSP_SERVER_PATH / __ABAP_LSP_SERVER_DEBUG) and uses LSP over stdio.
 * Development: start `abap_lsp_server --listen host:port` under a debugger, then set `serverTransport` "tcp" or `__ABAP_LSP_CONNECT=host:port` so the extension connects without spawning.
 */
function buildServerOptions(): ServerOptions {
	const config = vscode.workspace.getConfiguration("abap-ls");
	const connectOverride = process.env.__ABAP_LSP_CONNECT?.trim();
	const useTcp =
		Boolean(connectOverride) || config.get<string>("serverTransport") === "tcp";

	const tcpAddress =
		connectOverride ||
		config.get<string>("serverTcpAddress")?.trim() ||
		"127.0.0.1:9472";

	if (useTcp) {
		return () =>
			new Promise<StreamInfo>((resolve, reject) => {
				let connectOpts: net.SocketConnectOpts;
				try {
					connectOpts = parseHostPort(tcpAddress);
				} catch (err) {
					reject(err instanceof Error ? err : new Error(String(err)));
					return;
				}

				const socket = net.connect(connectOpts);
				socket.on("connect", () => {
					resolve({ writer: socket, reader: socket });
				});
				socket.on("error", reject);
			});
	}

	const pathFromEnv =
		process.env.__ABAP_LSP_SERVER_PATH?.trim() ||
		process.env.__ABAP_LSP_SERVER_DEBUG?.trim();
	const configured =
		pathFromEnv || config.get<string>("serverExecutable")?.trim();

	if (!configured) {
		void vscode.window.showErrorMessage(
			'ABAP LSP: no server executable configured for stdio transport. Set abap-ls.serverExecutable, or __ABAP_LSP_SERVER_PATH, or switch abap-ls.serverTransport to "tcp" and run the server with --listen.',
		);
		return () =>
			Promise.reject(new Error("ABAP LSP server executable path is not configured"));
	}

	const serverPath =
		process.platform === "win32" && path.extname(configured).length === 0
			? `${configured}.exe`
			: configured;

	return {
		command: serverPath,
		args: [],
		options: { cwd: path.dirname(serverPath) },
		transport: TransportKind.stdio,
	};
}

/** IPv4 / hostname and port only (e.g. 127.0.0.1:9472, localhost:9472). */
function parseHostPort(addr: string): net.SocketConnectOpts {
	const trimmed = addr.trim();
	const colon = trimmed.lastIndexOf(":");
	if (colon <= 0 || colon === trimmed.length - 1) {
		throw new Error(`Invalid TCP address "${addr}" (expected host:port)`);
	}
	const host = trimmed.slice(0, colon);
	const port = Number(trimmed.slice(colon + 1));
	if (!Number.isInteger(port) || port < 1 || port > 65535) {
		throw new Error(`Invalid port in TCP address "${addr}"`);
	}
	return { host, port };
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

function registerCommands(context: vscode.ExtensionContext): void {
	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.createLinkedProject", async () => {
			const workspaceFolder = await pickWorkspaceFolder();
			if (!workspaceFolder) {
				return;
			}

			await vscode.window.withProgress(
				{
					location: vscode.ProgressLocation.Notification,
					title: "Creating linked ABAP project",
				},
				async () => {
					await createLinkedProject(context, workspaceFolder);
				},
			);
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.addLocalWorkspaceObject", async () => {
			const workspaceFolder = await pickWorkspaceFolder();
			if (!workspaceFolder) {
				return;
			}

			await vscode.window.withProgress(
				{
					location: vscode.ProgressLocation.Notification,
					title: "Adding ABAP source file",
				},
				async () => {
					await promptAndCreateLocalWorkspaceObject(workspaceFolder);
				},
			);
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.configureSapConnection", async () => {
			const workspaceFolder = await pickWorkspaceFolder();
			if (!workspaceFolder) {
				return;
			}
			await configureSapConnection(context, workspaceFolder);
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.searchRepositoryObjects", async () => {
			const workspaceFolder = await pickWorkspaceFolder();
			if (!workspaceFolder) {
				return;
			}

			const objectRef = await promptForRepositoryObject(context, workspaceFolder);
			if (!objectRef) {
				return;
			}

			await vscode.window.showInformationMessage(
				`${objectRef.name} (${objectRef.type}) ${objectRef.packageName ? `in package ${objectRef.packageName}` : ""}`.trim(),
				"Copy ADT URI",
			).then(async (action) => {
				if (action === "Copy ADT URI") {
					await vscode.env.clipboard.writeText(objectRef.uri);
				}
			});
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.initializeWorkspaceFromAdtObject", async () => {
			const workspaceFolder = await pickWorkspaceFolder();
			if (!workspaceFolder) {
				return;
			}

			const objectRef = await promptForRepositoryObject(context, workspaceFolder);
			if (!objectRef) {
				return;
			}

			await vscode.window.withProgress(
				{
					location: vscode.ProgressLocation.Notification,
					title: `Pulling ${objectRef.name} from SAP`,
				},
				async () => {
					await addEditableAdtObjectToWorkspace(context, workspaceFolder, objectRef);
				},
			);
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.addEditableAdtObjectToWorkspace", async () => {
			const workspaceFolder = await pickWorkspaceFolder();
			if (!workspaceFolder) {
				return;
			}

			const objectRef = await promptForRepositoryObject(context, workspaceFolder);
			if (!objectRef) {
				return;
			}

			await vscode.window.withProgress(
				{
					location: vscode.ProgressLocation.Notification,
					title: `Adding ${objectRef.name} to workspace`,
				},
				async () => {
					await addEditableAdtObjectToWorkspace(context, workspaceFolder, objectRef);
				},
			);
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.createWorkspaceManifest", async () => {
			const workspaceFolder = await pickWorkspaceFolder();
			if (!workspaceFolder) {
				return;
			}

			await createWorkspaceManifest(workspaceFolder, { openManifest: true });
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.refreshDependencyCache", async () => {
			const workspaceFolder = await pickWorkspaceFolder();
			if (!workspaceFolder) {
				return;
			}

			const cacheDir = path.join(workspaceFolder.uri.fsPath, ".abapls", "cache");
			await fs.promises.rm(cacheDir, { recursive: true, force: true });
			await fs.promises.mkdir(cacheDir, { recursive: true });
			clearRemoteDependencyCaches(workspaceFolder);
			await client.sendNotification("abapls/dependencyCacheCleared", {
				workspaceUri: workspaceFolder.uri.toString(),
			} satisfies WorkspaceManifestUpdatedParams);
			vscode.window.showInformationMessage("ABAP LSP dependency cache cleared.");
		}),
	);
}

function registerClientNotifications(context: vscode.ExtensionContext): void {
	client.onNotification(
		"abapls/resolveRemoteDependencies",
		(params: RemoteDependencyResolveParams) => {
			void resolveRemoteDependencies(context, params);
		},
	);
	client.onNotification(
		"abapls/workspaceAnalysisStatus",
		(params: WorkspaceAnalysisStatusParams) => {
			handleWorkspaceAnalysisStatus(params);
		},
	);
}

function registerWorkspaceConfigPrompts(context: vscode.ExtensionContext): void {
	context.subscriptions.push(
		vscode.workspace.onDidOpenTextDocument((document) => {
			void maybePromptToCreateWorkspaceManifest(document);
		}),
	);
	context.subscriptions.push(
		vscode.workspace.onDidSaveTextDocument((document) => {
			if (path.basename(document.uri.fsPath) !== manifestFileName) {
				return;
			}
			const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
			if (!workspaceFolder) {
				return;
			}
			void notifyWorkspaceManifestUpdated(workspaceFolder);
		}),
	);

	for (const document of vscode.workspace.textDocuments) {
		void maybePromptToCreateWorkspaceManifest(document);
	}
}

function handleWorkspaceAnalysisStatus(params: WorkspaceAnalysisStatusParams): void {
	if (!params?.workspaceUri) {
		return;
	}

	if (params.phase === "started") {
		beginWorkspaceAnalysisProgress(params);
		return;
	}

	if (params.phase === "progress") {
		reportWorkspaceAnalysisProgress(params);
		return;
	}

	finishWorkspaceAnalysisProgress(params);
}

function beginWorkspaceAnalysisProgress(params: WorkspaceAnalysisStatusParams): void {
	const existing = workspaceAnalysisProgress.get(params.workspaceUri);
	if (existing) {
		if (existing.showTimer) {
			clearTimeout(existing.showTimer);
		}
		if (existing.resolve) {
			existing.resolve();
		}
	}

	const handle: WorkspaceAnalysisProgressHandle = {};
	handle.latest = params;
	handle.showTimer = setTimeout(() => {
		handle.showTimer = undefined;
		void vscode.window.withProgress(
			{
				location: vscode.ProgressLocation.Window,
				title: workspaceAnalysisProgressTitle(params),
				cancellable: false,
			},
			(progress) =>
				new Promise<void>((resolve) => {
					handle.resolve = resolve;
					handle.report = (nextParams) => {
						handle.latest = nextParams;
						progress.report({
							message: workspaceAnalysisProgressMessage(nextParams),
						});
					};
					if (handle.latest) {
						handle.report(handle.latest);
					}
				}),
		);
	}, 250);

	workspaceAnalysisProgress.set(params.workspaceUri, handle);
}

function reportWorkspaceAnalysisProgress(params: WorkspaceAnalysisStatusParams): void {
	const handle = workspaceAnalysisProgress.get(params.workspaceUri);
	if (!handle) {
		beginWorkspaceAnalysisProgress(params);
		return;
	}
	handle.latest = params;
	handle.report?.(params);
}

function finishWorkspaceAnalysisProgress(params: WorkspaceAnalysisStatusParams): void {
	const handle = workspaceAnalysisProgress.get(params.workspaceUri);
	if (handle?.showTimer) {
		clearTimeout(handle.showTimer);
	}
	if (handle?.resolve) {
		handle.resolve();
	}
	workspaceAnalysisProgress.delete(params.workspaceUri);

	const workspaceFolder = workspaceFolderForUri(params.workspaceUri);
	const workspaceLabel = workspaceFolder?.name ?? "workspace";
	const analyzedCount = params.analyzedDocumentCount;
	if (params.remoteResolutionInFlight) {
		vscode.window.setStatusBarMessage(
			`ABAP: analyzed ${analyzedCount} file${analyzedCount === 1 ? "" : "s"} in ${workspaceLabel}; fetching dependencies...`,
			5000,
		);
		return;
	}

	vscode.window.setStatusBarMessage(
		`ABAP: analyzed ${analyzedCount} file${analyzedCount === 1 ? "" : "s"} in ${workspaceLabel}; navigation and IntelliSense are ready.`,
		5000,
	);
}

function workspaceAnalysisProgressTitle(params: WorkspaceAnalysisStatusParams): string {
	const workspaceFolder = workspaceFolderForUri(params.workspaceUri);
	const workspaceLabel = workspaceFolder?.name ?? "workspace";
	switch (params.trigger) {
		case "manifest-updated":
			return `ABAP: refreshing ${workspaceLabel} after manifest change`;
		case "dependency-cache-cleared":
			return `ABAP: rebuilding ${workspaceLabel} after cache reset`;
		case "remote-dependencies-updated":
			return `ABAP: refreshing ${workspaceLabel} after dependency fetch`;
		default:
			return `ABAP: analyzing ${workspaceLabel}`;
	}
}

function formatWorkspaceDocumentCount(value: number): string {
	return Math.max(0, Math.trunc(value)).toLocaleString();
}

function workspaceAnalysisProgressMessage(params: WorkspaceAnalysisStatusParams): string {
	const total = Math.max(0, Math.trunc(params.totalDocumentCount));
	const processedRaw = Math.max(0, Math.trunc(params.processedDocumentCount));
	if (total > 0) {
		const stageTotal = Math.max(1, Math.floor(total / 2));
		if (processedRaw <= stageTotal) {
			const loaded = Math.min(processedRaw, stageTotal);
			const remaining = Math.max(stageTotal - loaded, 0);
			const percent = Math.min(100, Math.floor((loaded / stageTotal) * 100));
			return `Loading ${formatWorkspaceDocumentCount(loaded)}/${formatWorkspaceDocumentCount(stageTotal)} files (${percent}%), ${formatWorkspaceDocumentCount(remaining)} left before analysis`;
		}
		const analyzed = Math.min(processedRaw - stageTotal, stageTotal);
		const remaining = Math.max(stageTotal - analyzed, 0);
		const percent = Math.min(100, Math.floor((analyzed / stageTotal) * 100));
		return `Analyzing ${formatWorkspaceDocumentCount(analyzed)}/${formatWorkspaceDocumentCount(stageTotal)} files (${percent}%), ${formatWorkspaceDocumentCount(remaining)} left`;
	}
	if (processedRaw > 0) {
		return `${formatWorkspaceDocumentCount(processedRaw)} files processed`;
	}
	switch (params.trigger) {
		case "manifest-updated":
			return "Loading updated manifest and preparing document analysis...";
		case "dependency-cache-cleared":
			return "Reloading dependency cache and preparing document analysis...";
		case "remote-dependencies-updated":
			return "Applying fetched dependencies and preparing follow-up analysis...";
		default:
			return "Loading workspace manifest and preparing document analysis...";
	}
}

async function resolveRemoteDependencies(
	context: vscode.ExtensionContext,
	params: RemoteDependencyResolveParams,
): Promise<void> {
	if (!params?.workspaceUri || !params.candidates?.length) {
		return;
	}

	const workspaceFolder = workspaceFolderForUri(params.workspaceUri);
	if (!workspaceFolder) {
		return;
	}

	const fetched: string[] = [];
	const fetchCandidates: RemoteDependencyCandidate[] = [];
	const logCandidates: RemoteDependencyCandidate[] = [];
	const unknownSymbolMode = normalizeUnknownSymbolMode(params.unknownSymbolMode);
	const candidates = dedupeRemoteDependencyCandidates(params.candidates);
	const sourceUris = params.sourceUris?.length ? params.sourceUris : [params.sourceUri];

	for (const candidate of candidates) {
		if (shouldLogUnknownSymbolCandidate(candidate, unknownSymbolMode)) {
			logCandidates.push(candidate);
			continue;
		}
		fetchCandidates.push(candidate);
	}

	if (logCandidates.length > 0) {
		for (const sourceUri of sourceUris) {
			await appendUnknownSymbolLog(workspaceFolder, sourceUri, logCandidates);
		}
	}

	if (fetchCandidates.length === 0) {
		return;
	}

	const connection = await getSapConnectionConfig(context, workspaceFolder, { promptIfMissing: false });
	if (!connection) {
		return;
	}

	const fetchPolicy: RemoteDependencyFetchPolicy = {
		remoteRequestParallelism: params.remoteRequestParallelism,
		remoteRequestsPerSecond: params.remoteRequestsPerSecond,
	};
	const scheduler = remoteDependencySchedulerForWorkspace(workspaceFolder, fetchPolicy);
	const adtClient = new AdtClient(connection, {
		beforeRequest: () => scheduler.beforeRequest(),
	});

	const total = fetchCandidates.length;
	const failed: RemoteDependencyCandidate[] = [];
	await vscode.window.withProgress(
		{
			location: vscode.ProgressLocation.Notification,
			title: `ABAP: fetching ${total} remote dependenc${total === 1 ? "y" : "ies"} from ADT`,
			cancellable: false,
		},
		async (progress) => {
			let completed = 0;
			const results = await Promise.all(
				fetchCandidates.map((candidate) =>
					scheduler.schedule(async () => {
						try {
							return await resolveRemoteDependencyCandidate(
								workspaceFolder,
								adtClient,
								candidate,
							);
						} finally {
							completed += 1;
							progress.report({
								message: `${completed}/${total}: ${candidate.name} (${candidate.kind})`,
							});
						}
					}),
				),
			);
			for (const result of results) {
				if (result.fetchedName) {
					fetched.push(result.fetchedName);
				} else {
					failed.push(result.candidate);
				}
			}
		},
	);

	if (fetched.length === 0 && failed.length === 0) {
		return;
	}

	const updateParams: RemoteDependenciesUpdatedParams = {
		workspaceUri: params.workspaceUri,
		sourceUri: params.sourceUri,
		sourceUris,
		fetched,
		failed,
	};
	await client.sendNotification("abapls/remoteDependenciesUpdated", updateParams);
}

async function maybePromptToCreateWorkspaceManifest(
	document: vscode.TextDocument,
): Promise<void> {
	if (document.languageId !== "abap" || document.uri.scheme !== "file") {
		return;
	}

	const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
	if (!workspaceFolder) {
		return;
	}

	const workspaceKey = workspaceFolder.uri.toString();
	if (
		pendingWorkspaceConfigPrompts.has(workspaceKey) ||
		dismissedWorkspaceConfigPrompts.has(workspaceKey)
	) {
		return;
	}

	const manifestPath = workspaceManifestPath(workspaceFolder);
	if (await fileExists(manifestPath)) {
		dismissedWorkspaceConfigPrompts.add(workspaceKey);
		return;
	}

	pendingWorkspaceConfigPrompts.add(workspaceKey);
	try {
		const createAction = `Create ${manifestFileName}`;
		const action = await vscode.window.showInformationMessage(
			`Create a local-first ${manifestFileName} in "${workspaceFolder.name}"?`,
			createAction,
			"Not now",
		);

		if (action === createAction) {
			await createWorkspaceManifest(workspaceFolder, { openManifest: true });
			dismissedWorkspaceConfigPrompts.add(workspaceKey);
			return;
		}

		if (action === "Not now") {
			dismissedWorkspaceConfigPrompts.add(workspaceKey);
		}
	} finally {
		pendingWorkspaceConfigPrompts.delete(workspaceKey);
	}
}

async function createWorkspaceManifest(
	workspaceFolder: vscode.WorkspaceFolder,
	options: { openManifest?: boolean } = {},
): Promise<void> {
	const manifestPath = workspaceManifestPath(workspaceFolder);
	const alreadyExists = await fileExists(manifestPath);
	const manifestUri = await ensureWorkspaceManifest(workspaceFolder, {
		dependencyMode: dependencyModeLocalFirst,
		unknownSymbolMode: unknownSymbolModeLog,
	});
	await notifyWorkspaceManifestUpdated(workspaceFolder);
	dismissedWorkspaceConfigPrompts.add(workspaceFolder.uri.toString());

	if (options.openManifest) {
		const document = await vscode.workspace.openTextDocument(manifestUri);
		await vscode.window.showTextDocument(document, { preview: false });
	}

	void vscode.window.showInformationMessage(
		alreadyExists
			? `${manifestFileName} already exists. Use dependency_mode = "remote-on-demand" and unknown_symbol_mode = "remote" to enable ADT dependency fetches later.`
			: `Created local-first ${manifestFileName}. Set dependency_mode = "remote-on-demand" and unknown_symbol_mode = "remote" to enable ADT dependency fetches later, or keep unknown_symbol_mode = "log" to capture candidates in ${unknownSymbolLogPath}.`,
	);
}

async function createLinkedProject(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
): Promise<void> {
	const connection = await getSapConnectionConfig(context, workspaceFolder);
	if (!connection) {
		return;
	}

	await ensureWorkspaceManifest(workspaceFolder, {
		dependencyMode: dependencyModeLocalFirst,
		unknownSymbolMode: unknownSymbolModeLog,
	});
	await notifyWorkspaceManifestUpdated(workspaceFolder);
	dismissedWorkspaceConfigPrompts.add(workspaceFolder.uri.toString());

	const selection = await vscode.window.showQuickPick(
		[
			{ label: "Create first source file", createFirstObject: true },
			{ label: "Manifest only", createFirstObject: false },
		],
		{
			placeHolder: "Create the first local development object now?",
		},
	);
	if (!selection) {
		return;
	}

	if (selection.createFirstObject) {
		await promptAndCreateLocalWorkspaceObject(workspaceFolder);
		return;
	}

	void vscode.window.showInformationMessage(
		`Created linked ABAP project in "${workspaceFolder.name}".`,
	);
}

async function addEditableAdtObjectToWorkspace(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
): Promise<void> {
	if (!isSupportedEditableWorkspaceObject(objectRef)) {
		throw new Error(`Unsupported editable object type for ${objectRef.name} (${objectRef.type}).`);
	}
	if (objectRef.type.toUpperCase() === "FUGR/F" || isFunctionModuleObject(objectRef)) {
		await addEditableFunctionGroupToWorkspace(context, workspaceFolder, objectRef);
		return;
	}
	if (!isCustomEditableObjectName(objectRef.name)) {
		throw new Error(`Only customer objects with Z/Y prefixes or customer namespaces can be added to src: ${objectRef.name}.`);
	}

	const filePath = targetWorkspaceFilePath(workspaceFolder, objectRef.name);
	const cachedPath = targetDependencyWorkspaceFilePath(workspaceFolder, objectRef);
	const relativeFile = path.relative(workspaceFolder.uri.fsPath, filePath);
	await fs.promises.mkdir(path.dirname(filePath), { recursive: true });

	let source: string;
	let fileExisted = false;
	if (await fileExists(filePath)) {
		source = await fs.promises.readFile(filePath, "utf8");
		fileExisted = true;
	} else if (await fileExists(cachedPath)) {
		await fs.promises.mkdir(path.dirname(filePath), { recursive: true });
		await fs.promises.rename(cachedPath, filePath);
		source = await fs.promises.readFile(filePath, "utf8");
	} else {
		const connection = await getSapConnectionConfig(context, workspaceFolder);
		if (!connection) {
			return;
		}

		const adtClient = new AdtClient(connection);
		source = await adtClient.fetchObjectSource(objectRef.uri);
		await fs.promises.writeFile(filePath, source, "utf8");
		await adtClient.cacheRemoteObject(workspaceFolder, objectRef, source);
	}

	const manifestSpec = inferManifestUnitSpec(objectRef, relativeFile);
	await ensureManifestUnit(workspaceFolder, manifestSpec);
	// Server only loads abapls.toml at workspace init or on this notification;
	// without it, remote-on-demand resolution stays disabled until restart.
	await notifyWorkspaceManifestUpdated(workspaceFolder);

	const document = await vscode.workspace.openTextDocument(vscode.Uri.file(filePath));
	await vscode.window.showTextDocument(document, { preview: false });

	if (!fileExisted) {
		void vscode.window.showInformationMessage(`Added ${objectRef.name} to src/.`);
	}
}

async function addEditableFunctionGroupToWorkspace(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
): Promise<void> {
	const functionGroupRef = editableFunctionGroupObjectRef(objectRef);
	if (!isCustomEditableObjectName(functionGroupRef.name)) {
		throw new Error(
			`Only customer objects with Z/Y prefixes or customer namespaces can be added to src: ${functionGroupRef.name}.`,
		);
	}

	const connection = await getSapConnectionConfig(context, workspaceFolder);
	if (!connection) {
		return;
	}

	const adtClient = new AdtClient(connection);
	const groupChildren = await adtClient.listFunctionGroupChildren(functionGroupRef.name);
	const layout = editableFunctionGroupLayout(workspaceFolder, functionGroupRef, groupChildren, objectRef);
	await fs.promises.mkdir(layout.baseDir, { recursive: true });

	const createdFiles: string[] = [];
	for (const member of layout.members) {
		if (await fileExists(member.filePath)) {
			continue;
		}

		await fs.promises.mkdir(path.dirname(member.filePath), { recursive: true });
		const source = await adtClient.fetchObjectSource(member.objectRef.uri);
		await fs.promises.writeFile(member.filePath, source, "utf8");
		await adtClient.cacheRemoteObject(workspaceFolder, member.objectRef, source);
		createdFiles.push(member.filePath);
	}

	await ensureManifestUnit(workspaceFolder, layout.manifestUnit);
	await notifyWorkspaceManifestUpdated(workspaceFolder);

	const openPath = layout.openMember?.filePath ?? layout.rootFilePath;
	const document = await vscode.workspace.openTextDocument(vscode.Uri.file(openPath));
	await vscode.window.showTextDocument(document, { preview: false });

	if (createdFiles.length > 0) {
		void vscode.window.showInformationMessage(
			`Added function group ${functionGroupRef.name} to src/function-groups/.`,
		);
	}
}

function editableFunctionGroupObjectRef(objectRef: AdtObjectRef): AdtObjectRef {
	if (objectRef.type.toUpperCase() === "FUGR/F") {
		return {
			...objectRef,
			name: normalizedAdtObjectName(objectRef.name),
		};
	}

	const functionGroupUri = inferFunctionGroupUri(objectRef);
	if (!functionGroupUri) {
		throw new Error(`Cannot derive function group for ${objectRef.name}.`);
	}

	return {
		uri: functionGroupUri,
		type: "FUGR/F",
		name: normalizedAdtObjectName(lastAdtUriSegment(functionGroupUri)),
		packageName: objectRef.packageName,
		description: "Function group",
	};
}

function editableFunctionGroupLayout(
	workspaceFolder: vscode.WorkspaceFolder,
	functionGroupRef: AdtObjectRef,
	groupChildren: readonly AdtRepositoryChild[],
	selectedObjectRef: AdtObjectRef,
): {
	baseDir: string;
	rootFilePath: string;
	openMember?: EditableFunctionGroupMember;
	members: EditableFunctionGroupMember[];
	manifestUnit: ManifestUnitSpec;
} {
	const encodedGroupName = encodeURIComponent(functionGroupRef.name.trim().toUpperCase());
	const baseDir = path.join(workspaceFolder.uri.fsPath, "src", "function-groups", encodedGroupName);
	const rootFilePath = path.join(baseDir, `${encodedGroupName}.abap`);

	const includeChildren = groupChildren
		.filter((child) => child.objectRef.type.toUpperCase() === "FUGR/I")
		.sort((left, right) => left.objectRef.name.localeCompare(right.objectRef.name));
	const functionModuleChildren = groupChildren
		.filter((child) => child.objectRef.type.toUpperCase() === "FUGR/FF")
		.sort((left, right) => left.objectRef.name.localeCompare(right.objectRef.name));

	const members: EditableFunctionGroupMember[] = [
		{
			objectRef: functionGroupRef,
			role: "main",
			filePath: rootFilePath,
		},
		...includeChildren.map((child) => ({
			objectRef: child.objectRef,
			role: "root",
			filePath: path.join(
				baseDir,
				"includes",
				`${encodeURIComponent(normalizedAdtObjectName(child.objectRef.name))}.abap`,
			),
		})),
		...functionModuleChildren.map((child) => ({
			objectRef: child.objectRef,
			role: "root",
			filePath: path.join(
				baseDir,
				"function-modules",
				`${encodeURIComponent(normalizedAdtObjectName(child.objectRef.name))}.abap`,
			),
		})),
	];

	return {
		baseDir,
		rootFilePath,
		members,
		manifestUnit: {
			name: functionGroupRef.name,
			kind: "function-group",
			rootFile: path.relative(workspaceFolder.uri.fsPath, rootFilePath),
			adtUri: functionGroupRef.uri,
			role: "main",
			objectName: functionGroupRef.name,
			matchAdtUris: [
				functionGroupRef.uri,
				...includeChildren.map((child) => child.objectRef.uri),
				...functionModuleChildren.map((child) => child.objectRef.uri),
			],
			members: members.map((member) => ({
				role: member.role,
				file: path.relative(workspaceFolder.uri.fsPath, member.filePath),
				objectName: member.objectRef.name,
				adtUri: member.objectRef.uri,
			} satisfies ManifestUnitMemberSpec)),
		},
		openMember: members.find((member) =>
			member.objectRef.uri === selectedObjectRef.uri ||
			normalizedAdtObjectName(member.objectRef.name) === normalizedAdtObjectName(selectedObjectRef.name),
		),
	};
}

interface EditableFunctionGroupMember {
	objectRef: AdtObjectRef;
	role: string;
	filePath: string;
}

function normalizedAdtObjectName(name: string): string {
	return decodeURIComponent(name.trim()).toUpperCase();
}

function lastAdtUriSegment(uri: string): string {
	const trimmed = uri.replace(/\/+$/, "");
	const slashIndex = trimmed.lastIndexOf("/");
	return slashIndex >= 0 ? trimmed.slice(slashIndex + 1) : trimmed;
}

async function resolveRemoteDependencyCandidate(
	workspaceFolder: vscode.WorkspaceFolder,
	adtClient: AdtClient,
	candidate: RemoteDependencyCandidate,
): Promise<RemoteDependencyResolutionResult> {
	const cacheKey = remoteDependencyCacheKey(workspaceFolder, candidate);
	if (negativeRemoteDependencyCache.has(cacheKey)) {
		return { candidate };
	}
	if (await hasNegativeRemoteDependencyCandidate(workspaceFolder.uri.fsPath, candidate)) {
		negativeRemoteDependencyCache.add(cacheKey);
		return { candidate };
	}
	if (await hasCachedRemoteDependencyCandidate(workspaceFolder.uri.fsPath, candidate)) {
		return { candidate, fetchedName: candidate.name };
	}

	const existing = pendingRemoteDependencyFetches.get(cacheKey);
	if (existing) {
		const fetchedName = await existing;
		return { candidate, fetchedName };
	}

	const pending = (async () => {
		try {
			let objectRef;
			if (candidate.kind === "message-class") {
				objectRef = buildMessageClassObjectRef(candidate.name);
			} else {
				const objects = await adtClient.searchRepositoryObjects(candidate.name, 25);
				if (hasOnlyUnsupportedExactDomainMatches(candidate.name, objects)) {
					negativeRemoteDependencyCache.add(cacheKey);
					await markNegativeRemoteDependencyCandidate(
						workspaceFolder.uri.fsPath,
						candidate,
						"exact-match-domain-only",
					);
					return undefined;
				}
				objectRef = pickBestDependencyObject(candidate.name, objects, candidate.kind);
			}
			if (!objectRef) {
				negativeRemoteDependencyCache.add(cacheKey);
				await markNegativeRemoteDependencyCandidate(
					workspaceFolder.uri.fsPath,
					candidate,
					"no-supported-match",
				);
				return undefined;
			}

			const fetched = await adtClient.fetchDependencyObject(objectRef);
			const filePath = targetDependencyWorkspaceFilePath(workspaceFolder, objectRef);
			await fs.promises.mkdir(path.dirname(filePath), { recursive: true });
			await fs.promises.writeFile(filePath, fetched.body, "utf8");
			await ensureManifestDependencyUnit(workspaceFolder, objectRef, filePath);
			await adtClient.cacheRemoteObject(workspaceFolder, objectRef, fetched.body, fetched.fileExtension);
			return objectRef.name;
		} catch (error) {
			negativeRemoteDependencyCache.add(cacheKey);
			console.warn(`ABAP LSP remote dependency lookup failed for ${candidate.name}:`, error);
			return undefined;
		} finally {
			pendingRemoteDependencyFetches.delete(cacheKey);
		}
	})();

	pendingRemoteDependencyFetches.set(cacheKey, pending);
	const fetchedName = await pending;
	return { candidate, fetchedName };
}

async function promptForRepositoryObject(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
): Promise<AdtObjectRef | undefined> {
	const query = await vscode.window.showInputBox({
		prompt: "Search SAP repository objects",
		placeHolder: "ZCL_*",
		ignoreFocusOut: true,
	});
	if (!query?.trim()) {
		return undefined;
	}

	const connection = await getSapConnectionConfig(context, workspaceFolder);
	if (!connection) {
		return undefined;
	}

	const adtClient = new AdtClient(connection);
	const objects = await vscode.window.withProgress(
		{
			location: vscode.ProgressLocation.Notification,
			title: `Searching SAP repository for ${query.trim()}`,
		},
		() => adtClient.searchRepositoryObjects(query.trim()),
	);

	if (objects.length === 0) {
		vscode.window.showWarningMessage(`No ADT objects found for "${query.trim()}".`);
		return undefined;
	}

	const selection = await vscode.window.showQuickPick(
		objects.map((objectRef) => ({
			label: objectRef.name,
			description: `${objectRef.type} ${objectRef.packageName}`.trim(),
			detail: `${objectRef.description} ${objectRef.uri}`.trim(),
			objectRef,
		})),
		{
			matchOnDescription: true,
			matchOnDetail: true,
			placeHolder: "Select an ADT repository object",
		},
	);

	return selection?.objectRef;
}

async function promptAndCreateLocalWorkspaceObject(
	workspaceFolder: vscode.WorkspaceFolder,
): Promise<void> {
	const template = await pickLocalWorkspaceObjectTemplate();
	if (!template) {
		return;
	}

	const objectName = await promptForLocalWorkspaceObjectName(template);
	if (!objectName) {
		return;
	}

	await createLocalWorkspaceObject(workspaceFolder, template, objectName);
}

async function pickWorkspaceFolder(): Promise<vscode.WorkspaceFolder | undefined> {
	const folders = vscode.workspace.workspaceFolders ?? [];
	if (folders.length === 0) {
		vscode.window.showWarningMessage("Open a workspace folder first.");
		return undefined;
	}
	if (folders.length === 1) {
		return folders[0];
	}

	return vscode.window.showWorkspaceFolderPick({
		placeHolder: "Select the workspace folder for ABAP LSP commands",
	});
}

function workspaceFolderForUri(workspaceUri: string): vscode.WorkspaceFolder | undefined {
	const uri = vscode.Uri.parse(workspaceUri);
	return vscode.workspace.getWorkspaceFolder(uri) ??
		vscode.workspace.workspaceFolders?.find((folder) => folder.uri.toString() === workspaceUri);
}

function isCustomEditableObjectName(name: string): boolean {
	const trimmed = name.trim().toUpperCase();
	return trimmed.startsWith("Z") || trimmed.startsWith("Y");
}

async function pickLocalWorkspaceObjectTemplate(): Promise<LocalWorkspaceObjectTemplate | undefined> {
	const selection = await vscode.window.showQuickPick(
		localWorkspaceObjectTemplates().map((template) => ({
			label: template.label,
			description: `${template.kind} -> src/*.abap`,
			template,
		})),
		{
			placeHolder: "Select the development object type",
		},
	);
	return selection?.template;
}

async function promptForLocalWorkspaceObjectName(
	template: LocalWorkspaceObjectTemplate,
): Promise<string | undefined> {
	const objectName = (await vscode.window.showInputBox({
		prompt: `Name for the new ${template.label.toLowerCase()}`,
		placeHolder: template.namePlaceholder,
		ignoreFocusOut: true,
		validateInput: (value) => validateLocalWorkspaceObjectName(value, template),
	}))?.trim().toUpperCase();

	return objectName || undefined;
}

function validateLocalWorkspaceObjectName(
	value: string,
	template: LocalWorkspaceObjectTemplate,
): string | undefined {
	const normalized = value.trim().toUpperCase();
	if (!normalized) {
		return "Enter an ABAP object name.";
	}
	if (!isCustomEditableObjectName(normalized)) {
		return "Only customer objects with Z/Y prefixes or customer namespaces are supported.";
	}
	if (!template.namePattern.test(normalized)) {
		return `Use a name like ${template.namePlaceholder}.`;
	}
	return undefined;
}

export function validateLocalWorkspaceObjectNameForKind(
	value: string,
	kind: string,
): string | undefined {
	const template = localWorkspaceObjectTemplates().find((candidate) => candidate.kind === kind);
	if (!template) {
		return `Unsupported local workspace object kind: ${kind}`;
	}
	return validateLocalWorkspaceObjectName(value, template);
}

export function computeAbapFoldingRanges(text: string): AbapFoldingRangeShape[] {
	const lines = text.split(/\r?\n/);
	const ranges: AbapFoldingRangeShape[] = [];
	const stack: BlockEntry[] = [];

	for (let line = 0; line < lines.length; line += 1) {
		const code = lines[line].trim();
		if (!code || code.startsWith("\"")) {
			continue;
		}

		const keyword = leadingAbapKeyword(code);
		if (!keyword) {
			continue;
		}

		switch (keyword) {
			case "IF":
				stack.push({
					kind: "if",
					startLine: line,
					endKeyword: "ENDIF",
					currentArmStartLine: line,
				});
				break;
			case "ELSEIF":
			case "ELSE": {
				const entry = findNearestIf(stack);
				if (!entry) {
					break;
				}
				pushFoldingRange(ranges, entry.currentArmStartLine ?? entry.startLine, line - 1);
				entry.currentArmStartLine = line;
				break;
			}
			case "ENDIF": {
				const entry = popNearestBlock(stack, "if", "ENDIF");
				if (!entry) {
					break;
				}
				pushFoldingRange(ranges, entry.currentArmStartLine ?? entry.startLine, line - 1);
				break;
			}
			case "CLASS":
				if (!code.match(/^CLASS\b(?!-)/i)) {
					break;
				}
				stack.push({ kind: "simple", startLine: line, endKeyword: "ENDCLASS" });
				break;
			case "METHOD":
				stack.push({ kind: "simple", startLine: line, endKeyword: "ENDMETHOD" });
				break;
			case "CASE":
				stack.push({
					kind: "case",
					startLine: line,
					endKeyword: "ENDCASE",
				});
				break;
			case "WHEN":
			case "ELSE": {
				const caseEntry = findNearestCase(stack);
				if (caseEntry) {
					pushFoldingRange(
						ranges,
						caseEntry.currentArmStartLine ?? caseEntry.startLine,
						line - 1,
					);
					caseEntry.currentArmStartLine = line;
					break;
				}

				const ifEntry = findNearestIf(stack);
				if (!ifEntry) {
					break;
				}
				pushFoldingRange(ranges, ifEntry.currentArmStartLine ?? ifEntry.startLine, line - 1);
				ifEntry.currentArmStartLine = line;
				break;
			}
			case "ENDCASE": {
				const entry = popNearestBlock(stack, "case", "ENDCASE");
				if (!entry) {
					break;
				}
				pushFoldingRange(ranges, entry.currentArmStartLine ?? entry.startLine, line - 1);
				pushFoldingRange(ranges, entry.startLine, line - 1);
				break;
			}
			case "INTERFACE":
				if (!code.match(/^INTERFACE\b(?!-)/i)) {
					break;
				}
				stack.push({ kind: "simple", startLine: line, endKeyword: "ENDINTERFACE" });
				break;
			case "TRY":
				stack.push({ kind: "simple", startLine: line, endKeyword: "ENDTRY" });
				break;
			case "LOOP":
				stack.push({ kind: "simple", startLine: line, endKeyword: "ENDLOOP" });
				break;
			case "DO":
				stack.push({ kind: "simple", startLine: line, endKeyword: "ENDDO" });
				break;
			case "WHILE":
				stack.push({ kind: "simple", startLine: line, endKeyword: "ENDWHILE" });
				break;
			default: {
				const entry = popNearestSimpleBlock(stack, keyword);
				if (!entry) {
					break;
				}
				pushFoldingRange(ranges, entry.startLine, line - 1);
				break;
			}
		}
	}

	return ranges;
}

function leadingAbapKeyword(line: string): string | undefined {
	const match = line.match(/^([A-Z][A-Z0-9-]*)\b/i);
	return match?.[1].toUpperCase();
}

function findNearestIf(stack: BlockEntry[]): BlockEntry | undefined {
	for (let idx = stack.length - 1; idx >= 0; idx -= 1) {
		const entry = stack[idx];
		if (entry.kind === "if" && entry.endKeyword === "ENDIF") {
			return entry;
		}
	}
	return undefined;
}

function findNearestCase(stack: BlockEntry[]): BlockEntry | undefined {
	for (let idx = stack.length - 1; idx >= 0; idx -= 1) {
		const entry = stack[idx];
		if (entry.kind === "case" && entry.endKeyword === "ENDCASE") {
			return entry;
		}
	}
	return undefined;
}

function popNearestBlock(
	stack: BlockEntry[],
	kind: BlockEntry["kind"],
	endKeyword: string,
): BlockEntry | undefined {
	for (let idx = stack.length - 1; idx >= 0; idx -= 1) {
		const entry = stack[idx];
		if (entry.kind === kind && entry.endKeyword === endKeyword) {
			stack.splice(idx, 1);
			return entry;
		}
	}
	return undefined;
}

function popNearestSimpleBlock(stack: BlockEntry[], keyword: string): BlockEntry | undefined {
	for (let idx = stack.length - 1; idx >= 0; idx -= 1) {
		const entry = stack[idx];
		if (entry.kind === "simple" && entry.endKeyword === keyword) {
			stack.splice(idx, 1);
			return entry;
		}
	}
	return undefined;
}

function pushFoldingRange(
	ranges: AbapFoldingRangeShape[],
	start: number,
	end: number,
	kind?: vscode.FoldingRangeKind,
): void {
	if (end > start) {
		ranges.push({ start, end, kind });
	}
}

async function createLocalWorkspaceObject(
	workspaceFolder: vscode.WorkspaceFolder,
	template: LocalWorkspaceObjectTemplate,
	objectName: string,
): Promise<void> {
	await ensureWorkspaceManifest(workspaceFolder, {
		dependencyMode: dependencyModeLocalFirst,
		unknownSymbolMode: unknownSymbolModeLog,
	});

	const filePath = targetWorkspaceFilePath(workspaceFolder, objectName);
	if (await fileExists(filePath)) {
		const document = await vscode.workspace.openTextDocument(vscode.Uri.file(filePath));
		await vscode.window.showTextDocument(document, { preview: false });
		void vscode.window.showWarningMessage(
			`ABAP source already exists: ${path.relative(workspaceFolder.uri.fsPath, filePath)}`,
		);
		return;
	}

	await fs.promises.mkdir(path.dirname(filePath), { recursive: true });
	await fs.promises.writeFile(filePath, template.stub(objectName), "utf8");
	dismissedWorkspaceConfigPrompts.add(workspaceFolder.uri.toString());

	await ensureManifestUnit(workspaceFolder, {
		name: objectName,
		kind: template.kind,
		rootFile: path.relative(workspaceFolder.uri.fsPath, filePath),
		role: template.role,
		objectName,
	});
	await notifyWorkspaceManifestUpdated(workspaceFolder);

	const document = await vscode.workspace.openTextDocument(vscode.Uri.file(filePath));
	await vscode.window.showTextDocument(document, { preview: false });
	void vscode.window.showInformationMessage(
		`Added ${template.label.toLowerCase()} ${objectName} to "${workspaceFolder.name}".`,
	);
}

function isSupportedEditableWorkspaceObject(objectRef: AdtObjectRef): boolean {
	if (objectRef.type.startsWith("CLAS/") || objectRef.type.startsWith("INTF/")) {
		return true;
	}

	const loweredUri = objectRef.uri.toLowerCase();
	const normalizedType = objectRef.type.toUpperCase();
	return loweredUri.includes("/programs/includes/") ||
		loweredUri.includes("/programs/programs/") ||
		loweredUri.includes("/functions/groups/") ||
		normalizedType === "PROG/I" ||
		normalizedType === "PROG/P";
}

function localWorkspaceObjectTemplates(): LocalWorkspaceObjectTemplate[] {
	return [
		{
			label: "Report",
			kind: "report",
			role: "root",
			namePattern: customerObjectNamePattern,
			namePlaceholder: "ZMY_NEW_REPORT",
			stub: (name) => `REPORT ${name}.\n`,
		},
		{
			label: "Include",
			kind: "include",
			role: "root",
			namePattern: customerObjectNamePattern,
			namePlaceholder: "ZMY_NEW_INCLUDE",
			stub: (name) => `* Include ${name}\n`,
		},
		{
			label: "Global Class",
			kind: "global-class",
			role: "main",
			namePattern: customerObjectNamePattern,
			namePlaceholder: "ZCL_MY_NEW_CLASS",
			stub: (name) => `CLASS ${name} DEFINITION PUBLIC FINAL CREATE PUBLIC.\nENDCLASS.\n\nCLASS ${name} IMPLEMENTATION.\nENDCLASS.\n`,
		},
		{
			label: "Global Interface",
			kind: "global-interface",
			role: "main",
			namePattern: customerObjectNamePattern,
			namePlaceholder: "ZIF_MY_NEW_INTERFACE",
			stub: (name) => `INTERFACE ${name} PUBLIC.\nENDINTERFACE.\n`,
		},
	];
}

function remoteDependencyCacheKey(
	workspaceFolder: vscode.WorkspaceFolder,
	candidate: RemoteDependencyCandidate,
): string {
	return `${workspaceFolder.uri.toString()}:${candidate.name.toLowerCase()}`;
}

function clearRemoteDependencyCaches(workspaceFolder: vscode.WorkspaceFolder): void {
	const prefix = `${workspaceFolder.uri.toString()}:`;
	for (const key of negativeRemoteDependencyCache) {
		if (key.startsWith(prefix)) {
			negativeRemoteDependencyCache.delete(key);
		}
	}
	for (const key of pendingRemoteDependencyFetches.keys()) {
		if (key.startsWith(prefix)) {
			pendingRemoteDependencyFetches.delete(key);
		}
	}
	remoteDependencySchedulers.delete(workspaceFolder.uri.toString());
}

function normalizeUnknownSymbolMode(value: string | undefined): string {
	return value?.trim().toLowerCase() === unknownSymbolModeLog
		? unknownSymbolModeLog
		: unknownSymbolModeRemote;
}

function shouldLogUnknownSymbolCandidate(
	candidate: RemoteDependencyCandidate,
	unknownSymbolMode: string,
): boolean {
	return unknownSymbolMode === unknownSymbolModeLog && candidate.kind !== "include";
}

async function appendUnknownSymbolLog(
	workspaceFolder: vscode.WorkspaceFolder,
	sourceUri: string,
	candidates: RemoteDependencyCandidate[],
): Promise<void> {
	const dedupedCandidates = dedupeRemoteDependencyCandidates(candidates);
	if (dedupedCandidates.length === 0) {
		return;
	}

	const logPath = path.join(workspaceFolder.uri.fsPath, unknownSymbolLogPath);
	const timestamp = new Date().toISOString();
	const lines = dedupedCandidates.map((candidate) =>
		`${timestamp}\t${sourceUri}\t${candidate.kind}\t${candidate.name}\n`,
	);

	await fs.promises.mkdir(path.dirname(logPath), { recursive: true });
	await fs.promises.appendFile(logPath, lines.join(""), "utf8");
}

function remoteDependencySchedulerForWorkspace(
	workspaceFolder: vscode.WorkspaceFolder,
	policy: RemoteDependencyFetchPolicy,
): RemoteDependencyScheduler {
	const key = workspaceFolder.uri.toString();
	let scheduler = remoteDependencySchedulers.get(key);
	if (!scheduler) {
		scheduler = new RemoteDependencyScheduler();
		remoteDependencySchedulers.set(key, scheduler);
	}

	scheduler.updatePolicy(policy);
	return scheduler;
}

async function notifyWorkspaceManifestUpdated(
	workspaceFolder: vscode.WorkspaceFolder,
): Promise<void> {
	const params: WorkspaceManifestUpdatedParams = {
		workspaceUri: workspaceFolder.uri.toString(),
	};

	await client.sendNotification("abapls/workspaceManifestUpdated", params);
}

async function fileExists(filePath: string): Promise<boolean> {
	try {
		await fs.promises.access(filePath, fs.constants.F_OK);
		return true;
	} catch {
		return false;
	}
}

// function getPythonCommand(): string {
//   const config = workspace.getConfiguration("abap-ls");
//   const configured = config.get<string>("pythonPath");

//   if (configured && configured.trim()) {
//     return configured.trim();
//   }

//   // Fallbacks if user didn't configure pythonPath
//   if (process.platform === "win32") {
//     return "python"; // or "py" depending on your setup
//   }
//   return "python3";
// }
