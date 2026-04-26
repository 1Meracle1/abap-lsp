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
	AdtDependencyFetchResult,
	AdtRequestFinishedEvent,
	AdtRequestStartEvent,
	AdtRepositoryChild,
	AdtObjectRef,
	AdtRequestCancelledError,
	configureSapConnection,
	directDependencyObjectRefs,
	inferFunctionGroupUri,
	inferLocalExportObjectRef,
	isDdicDependencyObject,
	isFunctionModuleObject,
	isMessageClassDependencyObject,
	getSapConnectionConfig,
	hasOnlyUnsupportedExactDomainMatches,
	parseLocalDdicExportObjectRef,
	selectDependencyObjects,
} from "./adt";
import {
	dependencyModeRemoteOnDemand,
	ensureWorkspaceManifest,
	inferManifestUnitSpec,
	manifestFileName,
	targetEditableWorkspaceFilePath,
	workspaceManifestPath,
} from "./manifest";
import {
	dedupeRemoteDependencyCandidates,
	RemoteDependencyCandidate,
	RemoteDependencyFetchPolicy,
	RemoteDependencyScheduler,
	resolveRemoteDependencyFetchPolicy,
} from "./remoteDependencies";
import {
	clearLocalExportIndexCache,
	findLocalExportFileInIndexedRoot,
} from "./localExportIndex";

let client: LanguageClient;
let adtRequestGeneration = 0;
let clientLifecycle = Promise.resolve();
let workspaceAnalysisStatusBarMessage: vscode.Disposable | undefined;
const pendingRemoteDependencyFetches = new Map<string, Promise<RemoteDependencyResolutionResult>>();
const negativeRemoteDependencyCache = new Set<string>();
const remoteDependencySchedulers = new Map<string, RemoteDependencyScheduler>();
const pendingWorkspaceConfigPrompts = new Set<string>();
const dismissedWorkspaceConfigPrompts = new Set<string>();
const workspaceAnalysisProgress = new Map<string, WorkspaceAnalysisProgressHandle>();
type UnitDependencySourceMode = "local-first" | "local-only" | "adt-first";

interface RemoteDependencyResolveParams {
	workspaceUri: string;
	sourceUri: string;
	sourceUris?: string[];
	sourceCandidates?: Record<string, RemoteDependencyCandidate[]>;
	retryNegativeCandidates?: boolean;
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
	failed?: boolean;
}

interface RemoteDependencyBatchContext {
	centralArtifacts: DependencyArtifactPayload[];
	negativeCandidates: RemoteDependencyCandidate[];
	sourceUnitSidecarPathsByKey: Map<string, Promise<string[]>>;
	localDependencyRootsByKey: Map<string, Promise<string[]>>;
	dependencySourceModeByKey: Map<string, Promise<UnitDependencySourceMode>>;
	localRootsBySidecarPath: Map<string, Promise<string[]>>;
	dependencySourceModeBySidecarPath: Map<string, Promise<UnitDependencySourceMode | undefined>>;
}

interface DependencyCacheInitializationOptions {
	dependencyCachePath?: string;
}

interface DependencyArtifactPayload {
	packageName: string;
	objectKind: string;
	objectName: string;
	objectUri: string;
	objectType: string;
	description: string;
	fileExtension: "abap" | "xml";
	sourceText: string;
	fetchedAt: string;
}

interface StoreRemoteDependencyArtifactsParams {
	workspaceUri: string;
	connectionKey?: string;
	artifacts: DependencyArtifactPayload[];
	negative: RemoteDependencyCandidate[];
}

interface ReadDependencyDocumentResult {
	sourceText: string;
}

interface RemoteDependencyWaveTelemetry {
	workspaceLabel: string;
	totalCandidates: number;
	configuredRequestsPerSecond: number;
	configuredParallelism: number;
	startedAt: number;
	completedCandidates: number;
	requestsStarted: number;
	requestsFinished: number;
	requestsFailed: number;
	directFetchCandidates: number;
	searchCandidates: number;
	requestKinds: Map<string, number>;
	progressTimer?: NodeJS.Timeout;
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

type EditableAdtObjectTarget =
	| { kind: "file"; filePath: string }
	| { kind: "directory"; directoryPath: string };

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
		{ scheme: "abapls-cache", language: "abap" },
	];
	const foldingDocumentSelector: vscode.DocumentFilter[] = [
		{ scheme: "file", language: "abap" },
		{ scheme: "untitled", language: "abap" },
		{ scheme: "abapls-cache", language: "abap" },
	];

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: clientDocumentSelector,
		initializationOptions: {
			get dependencyCachePath(): string | undefined {
				return configuredDependencyCachePath();
			},
		} satisfies DependencyCacheInitializationOptions,
		synchronize: {
			fileEvents: [
				vscode.workspace.createFileSystemWatcher("**/.clientrc"),
				vscode.workspace.createFileSystemWatcher("**/abapls.toml"),
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
		vscode.workspace.registerTextDocumentContentProvider("abapls-cache", {
			async provideTextDocumentContent(uri) {
				if (!client.isRunning()) {
					return `* ABAP LSP dependency document is unavailable because the language server is not running.\n* ${uri.toString()}`;
				}
				try {
					const result = await client.sendRequest<ReadDependencyDocumentResult | null>(
						"abapls/readDependencyDocument",
						{ uri: uri.toString(true) },
					);
					if (result !== null) {
						return result.sourceText;
					}
				} catch (error) {
					client.outputChannel.appendLine(
						`[dependency-doc] ${uri.toString()}: ${error instanceof Error ? error.message : String(error)}`,
					);
				}
				return `* ABAP LSP could not load this cached dependency document.\n* ${uri.toString()}`;
			},
		}),
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
	registerConfigurationListeners(context);

	// Start the client. This will also launch the server
	void startLanguageClient();
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

function describeServerConnection(): string {
	const config = vscode.workspace.getConfiguration("abap-ls");
	const connectOverride = process.env.__ABAP_LSP_CONNECT?.trim();
	const useTcp =
		Boolean(connectOverride) || config.get<string>("serverTransport") === "tcp";
	if (useTcp) {
		return `tcp:${connectOverride || config.get<string>("serverTcpAddress")?.trim() || "127.0.0.1:9472"}`;
	}

	const pathFromEnv =
		process.env.__ABAP_LSP_SERVER_PATH?.trim() ||
		process.env.__ABAP_LSP_SERVER_DEBUG?.trim();
	const configured =
		pathFromEnv || config.get<string>("serverExecutable")?.trim() || "<unset>";
	const serverPath =
		process.platform === "win32" && path.extname(configured).length === 0
			? `${configured}.exe`
			: configured;
	return `stdio:${serverPath}`;
}

function configuredDependencyCachePath(): string | undefined {
	const configured = vscode.workspace
		.getConfiguration("abap-ls")
		.get<string>("dependencyCache.path")
		?.trim();
	return configured || undefined;
}

function registerConfigurationListeners(context: vscode.ExtensionContext): void {
	context.subscriptions.push(
		vscode.workspace.onDidChangeConfiguration((event) => {
			if (!event.affectsConfiguration("abap-ls.dependencyCache.path")) {
				return;
			}
			void runClientLifecycle(async () => {
				cancelAllAdtFetches();
				clearProgressUi();
				await stopLanguageClient();
				await startLanguageClient();
			});
		}),
	);
}

function logSemanticTokenEditorContext(
	editor: vscode.TextEditor | undefined,
): void {
	if (!editor) {
		client.outputChannel.appendLine("[startup] active editor: <none>");
		return;
	}
	const semanticHighlighting = vscode.workspace
		.getConfiguration("editor", editor.document.uri)
		.get("semanticHighlighting.enabled");
	client.outputChannel.appendLine(
		`[startup] active editor: language=${editor.document.languageId} uri=${editor.document.uri.toString()} semanticHighlighting.enabled=${String(
			semanticHighlighting,
		)}`,
	);
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
	cancelAllAdtFetches();
	clearProgressUi();
	return client.isRunning() ? client.stop() : Promise.resolve();
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
		vscode.commands.registerCommand("abap-ls.addEditableAdtObjectToWorkspace", async () => {
			const workspaceFolder = await pickWorkspaceFolder();
			if (!workspaceFolder) {
				return;
			}

			const objectRef = await promptForRepositoryObject(context, workspaceFolder);
			if (!objectRef) {
				return;
			}

			const target = await promptForEditableAdtObjectTarget(workspaceFolder, objectRef);
			if (!target) {
				return;
			}

			await vscode.window.withProgress(
				{
					location: vscode.ProgressLocation.Notification,
					title: `Adding ${objectRef.name} to workspace`,
				},
				async () => {
					await addEditableAdtObjectToWorkspace(context, workspaceFolder, objectRef, target);
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

			clearRemoteDependencyCaches(workspaceFolder);
			await client.sendNotification("abapls/dependencyCacheRefreshRequested", {
				workspaceUri: workspaceFolder.uri.toString(),
			} satisfies WorkspaceManifestUpdatedParams);
			vscode.window.showInformationMessage("ABAP LSP dependency cache refresh requested.");
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.stopLanguageServer", async () => {
			await runClientLifecycle(async () => {
				cancelAllAdtFetches();
				clearProgressUi();
				await stopLanguageClient();
			});
			vscode.window.showInformationMessage("ABAP LSP language server stopped.");
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.restartLanguageServer", async () => {
			await runClientLifecycle(async () => {
				cancelAllAdtFetches();
				clearProgressUi();
				await stopLanguageClient();
				await startLanguageClient();
			});
			vscode.window.showInformationMessage("ABAP LSP language server restarted.");
		}),
	);
}

function createAdtClient(
	connection: NonNullable<Awaited<ReturnType<typeof getSapConnectionConfig>>>,
	options: {
		beforeRequest?: () => Promise<void>;
		onRequestStart?: (event: AdtRequestStartEvent) => void;
		onRequestFinished?: (event: AdtRequestFinishedEvent) => void;
	} = {},
): AdtClient {
	const generation = adtRequestGeneration;
	return new AdtClient(connection, {
		beforeRequest: async () => {
			throwIfAdtFetchesCancelled(generation);
			await options.beforeRequest?.();
			throwIfAdtFetchesCancelled(generation);
		},
		isCancelled: () => generation !== adtRequestGeneration,
		onRequestStart: options.onRequestStart,
		onRequestFinished: options.onRequestFinished,
	});
}

function throwIfAdtFetchesCancelled(generation: number): void {
	if (generation !== adtRequestGeneration) {
		throw new AdtRequestCancelledError();
	}
}

function cancelAllAdtFetches(): void {
	adtRequestGeneration += 1;
	AdtClient.cancelAllActiveRequests();
	for (const scheduler of remoteDependencySchedulers.values()) {
		scheduler.cancelAll();
	}
	pendingRemoteDependencyFetches.clear();
}

function clearProgressUi(): void {
	for (const handle of workspaceAnalysisProgress.values()) {
		if (handle.showTimer) {
			clearTimeout(handle.showTimer);
		}
		handle.resolve?.();
	}
	workspaceAnalysisProgress.clear();
	workspaceAnalysisStatusBarMessage?.dispose();
	workspaceAnalysisStatusBarMessage = undefined;
}

async function startLanguageClient(): Promise<void> {
	if (!client.needsStart()) {
		return;
	}
	await client.start();
	logLanguageClientStartup();
}

async function stopLanguageClient(): Promise<void> {
	if (!client.isRunning()) {
		return;
	}
	await client.stop();
}

function runClientLifecycle(action: () => Promise<void>): Promise<void> {
	const next = clientLifecycle
		.catch(() => undefined)
		.then(action);
	clientLifecycle = next.catch(() => undefined);
	return next;
}

function logLanguageClientStartup(): void {
	client.outputChannel.appendLine(
		`[startup] server connection: ${describeServerConnection()}`,
	);
	client.outputChannel.appendLine(
		`[startup] semanticTokensProvider: ${JSON.stringify(
			client.initializeResult?.capabilities?.semanticTokensProvider ?? null,
		)}`,
	);
	logSemanticTokenEditorContext(vscode.window.activeTextEditor);
}

function registerClientNotifications(context: vscode.ExtensionContext): void {
	client.onNotification(
		"abapls/resolveRemoteDependencies",
		(params: RemoteDependencyResolveParams) => {
			void resolveRemoteDependencies(context, params).catch((error) => {
				client.outputChannel.appendLine(
					`[remote-deps] unhandled resolver failure: ${error instanceof Error ? error.stack ?? error.message : String(error)}`,
				);
			});
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
		workspaceAnalysisStatusBarMessage?.dispose();
		workspaceAnalysisStatusBarMessage = vscode.window.setStatusBarMessage(
			`ABAP: analyzed ${analyzedCount} file${analyzedCount === 1 ? "" : "s"} in ${workspaceLabel}; fetching dependencies...`,
			5000,
		);
		return;
	}

	workspaceAnalysisStatusBarMessage?.dispose();
	workspaceAnalysisStatusBarMessage = vscode.window.setStatusBarMessage(
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
		case "dependency-cache-refresh":
			return `ABAP: refreshing ${workspaceLabel} dependency cache`;
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
	if (params.remoteResolutionInFlight && total > 0 && processedRaw >= total) {
		return "Document analysis is complete; resolving dependency waves...";
	}
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
		case "dependency-cache-refresh":
			return "Refreshing dependency cache and preparing document analysis...";
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

	const sourceUris = params.sourceUris?.length ? params.sourceUris : [params.sourceUri];
	const updateParams: RemoteDependenciesUpdatedParams = {
		workspaceUri: params.workspaceUri,
		sourceUri: params.sourceUri,
		sourceUris,
		fetched: [],
		failed: [],
	};
	const workspaceFolder = workspaceFolderForUri(params.workspaceUri);
	if (!workspaceFolder) {
		await sendRemoteDependenciesUpdatedSafe(updateParams, "workspace-missing");
		return;
	}

	const candidates = dedupeRemoteDependencyCandidates(params.candidates);
	const fetchCandidates = [...candidates];
	const candidateSourceUris = candidateSourceUriMap(params, sourceUris);
	const retryNegativeCandidates = shouldRetryNegativeRemoteDependencyCandidates(
		params.retryNegativeCandidates,
	);
	const batchContext = createRemoteDependencyBatchContext();
	const fetchPolicy: RemoteDependencyFetchPolicy = {
		remoteRequestParallelism: params.remoteRequestParallelism,
		remoteRequestsPerSecond: params.remoteRequestsPerSecond,
	};
	const resolvedFetchPolicy = resolveRemoteDependencyFetchPolicy(fetchPolicy);
	const telemetry = createRemoteDependencyWaveTelemetry(
		workspaceFolder,
		fetchCandidates.length,
		resolvedFetchPolicy.remoteRequestsPerSecond,
		resolvedFetchPolicy.remoteRequestParallelism,
	);
	startRemoteDependencyWaveTelemetry(telemetry);

	const scheduler = remoteDependencySchedulerForWorkspace(workspaceFolder, fetchPolicy);
	let adtClientPromise: Promise<AdtClient | undefined> | undefined;
	const getAdtClient = async (): Promise<AdtClient | undefined> => {
		if (adtClientPromise) {
			return adtClientPromise;
		}

		adtClientPromise = (async () => {
			const connection = await getSapConnectionConfig(context, workspaceFolder, { promptIfMissing: false });
			if (!connection) {
				client.outputChannel.appendLine(
					`[remote-deps] ${workspaceFolder.name}: skipped ADT fetch wave because no SAP connection was found in settings, environment, or .env`,
				);
				return undefined;
			}

			return createAdtClient(connection, {
				beforeRequest: () => scheduler.beforeRequest(),
				onRequestStart: (event) => {
					recordRemoteDependencyRequestStart(telemetry, event);
				},
				onRequestFinished: (event) => {
					recordRemoteDependencyRequestFinished(telemetry, event);
				},
			});
		})();

		return adtClientPromise;
	};

	let waveError: unknown;
	const fetched: string[] = [];
	const failed: RemoteDependencyCandidate[] = [];
	try {
		if (fetchCandidates.length > 0) {
			const total = fetchCandidates.length;
			try {
				await vscode.window.withProgress(
					{
						location: vscode.ProgressLocation.Notification,
						title: `ABAP: resolving ${total} external dependenc${total === 1 ? "y" : "ies"}`,
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
											getAdtClient,
											candidate,
											candidateSourceUris.get(remoteDependencyCandidateKey(candidate)) ?? sourceUris,
											batchContext,
											telemetry,
											retryNegativeCandidates,
										);
									} finally {
										completed += 1;
										recordRemoteDependencyCandidateCompleted(telemetry, completed);
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
							} else if (result.failed) {
								failed.push(result.candidate);
							}
						}
					},
				);
			} catch (error) {
				if (!(error instanceof AdtRequestCancelledError)) {
					throw error;
				}
				waveError = error;
			}
		}
	} catch (error) {
		waveError = error;
		client.outputChannel.appendLine(
			`[remote-deps] ${workspaceFolder.name}: dependency wave failed before completion: ${error instanceof Error ? error.stack ?? error.message : String(error)}`,
		);
	} finally {
		try {
			await flushPendingDependencyCacheUnits(workspaceFolder, batchContext);
		} catch (flushError) {
			waveError ??= flushError;
			client.outputChannel.appendLine(
				`[remote-deps] ${workspaceFolder.name}: failed to flush dependency cache updates: ${flushError instanceof Error ? flushError.stack ?? flushError.message : String(flushError)}`,
			);
		}
		updateParams.fetched = fetched;
		updateParams.failed = failed;
		finishRemoteDependencyWaveTelemetry(telemetry, updateParams, waveError);
		await sendRemoteDependenciesUpdatedSafe(updateParams, "wave-finished");
	}
}

async function sendRemoteDependenciesUpdatedSafe(
	params: RemoteDependenciesUpdatedParams,
	reason: string,
): Promise<void> {
	if (!client.isRunning()) {
		return;
	}
	try {
		await client.sendNotification("abapls/remoteDependenciesUpdated", params);
	} catch (error) {
		client.outputChannel.appendLine(
			`[remote-deps] failed to notify server (${reason}): ${error instanceof Error ? error.stack ?? error.message : String(error)}`,
		);
	}
}

function createRemoteDependencyWaveTelemetry(
	workspaceFolder: vscode.WorkspaceFolder,
	totalCandidates: number,
	configuredRequestsPerSecond: number,
	configuredParallelism: number,
): RemoteDependencyWaveTelemetry {
	return {
		workspaceLabel: workspaceFolder.name,
		totalCandidates,
		configuredRequestsPerSecond,
		configuredParallelism,
		startedAt: Date.now(),
		completedCandidates: 0,
		requestsStarted: 0,
		requestsFinished: 0,
		requestsFailed: 0,
		directFetchCandidates: 0,
		searchCandidates: 0,
		requestKinds: new Map(),
	};
}

function startRemoteDependencyWaveTelemetry(telemetry: RemoteDependencyWaveTelemetry): void {
	client.outputChannel.appendLine(
		`[remote-deps] ${telemetry.workspaceLabel}: wave start candidates=${telemetry.totalCandidates} configured_rps=${telemetry.configuredRequestsPerSecond} configured_parallelism=${telemetry.configuredParallelism}`,
	);
	telemetry.progressTimer = setInterval(() => {
		client.outputChannel.appendLine(
			`[remote-deps] ${telemetry.workspaceLabel}: wave progress candidates=${telemetry.completedCandidates}/${telemetry.totalCandidates} requests=${telemetry.requestsFinished}/${telemetry.requestsStarted} avg_req_per_s=${averageRemoteDependencyRequestsPerSecond(telemetry).toFixed(1)}`,
		);
	}, 5000);
}

function recordRemoteDependencyRequestStart(
	telemetry: RemoteDependencyWaveTelemetry,
	event: AdtRequestStartEvent,
): void {
	telemetry.requestsStarted += 1;
	const kind = classifyRemoteDependencyRequest(event.pathOrUrl);
	telemetry.requestKinds.set(kind, (telemetry.requestKinds.get(kind) ?? 0) + 1);
}

function recordRemoteDependencyRequestFinished(
	telemetry: RemoteDependencyWaveTelemetry,
	event: AdtRequestFinishedEvent,
): void {
	telemetry.requestsFinished += 1;
	if (event.error) {
		telemetry.requestsFailed += 1;
	}
}

function recordRemoteDependencyCandidateCompleted(
	telemetry: RemoteDependencyWaveTelemetry,
	completedCandidates: number,
): void {
	telemetry.completedCandidates = completedCandidates;
}

function recordRemoteDependencyDirectFetchCandidate(
	telemetry: RemoteDependencyWaveTelemetry,
): void {
	telemetry.directFetchCandidates += 1;
}

function recordRemoteDependencySearchCandidate(
	telemetry: RemoteDependencyWaveTelemetry,
): void {
	telemetry.searchCandidates += 1;
}

function finishRemoteDependencyWaveTelemetry(
	telemetry: RemoteDependencyWaveTelemetry,
	updateParams: RemoteDependenciesUpdatedParams,
	error: unknown,
): void {
	if (telemetry.progressTimer) {
		clearInterval(telemetry.progressTimer);
		telemetry.progressTimer = undefined;
	}
	const durationMs = Math.max(Date.now() - telemetry.startedAt, 1);
	const kinds = [...telemetry.requestKinds.entries()]
		.sort((left, right) => left[0].localeCompare(right[0]))
		.map(([kind, count]) => `${kind}=${count}`)
		.join(", ");
	client.outputChannel.appendLine(
		`[remote-deps] ${telemetry.workspaceLabel}: wave finished candidates=${telemetry.completedCandidates}/${telemetry.totalCandidates} fetched=${updateParams.fetched.length} failed=${updateParams.failed.length} direct=${telemetry.directFetchCandidates} searched=${telemetry.searchCandidates} requests=${telemetry.requestsFinished}/${telemetry.requestsStarted} request_failures=${telemetry.requestsFailed} duration=${formatRemoteDependencyDuration(durationMs)} avg_req_per_s=${averageRemoteDependencyRequestsPerSecond(telemetry).toFixed(1)}${kinds ? ` kinds=[${kinds}]` : ""}${error ? " status=error" : " status=ok"}`,
	);
}

function classifyRemoteDependencyRequest(pathOrUrl: string): string {
	const lower = pathOrUrl.toLowerCase();
	if (lower.includes("/sap/bc/adt/runtime/systemmessages")) {
		return "bootstrap";
	}
	if (lower.includes("/sap/bc/adt/repository/informationsystem/search")) {
		return "quick-search";
	}
	if (lower.includes("/sap/bc/adt/repository/nodestructure")) {
		return "node-structure";
	}
	if (lower.includes("/sap/bc/adt/messageclass/")) {
		return "message-class";
	}
	if (lower.includes("/sap/bc/adt/ddic/") || lower.includes("/sap/bc/adt/elementinfo")) {
		return "ddic";
	}
	if (lower.endsWith("/source/main")) {
		return "source";
	}
	return "other";
}

function averageRemoteDependencyRequestsPerSecond(
	telemetry: RemoteDependencyWaveTelemetry,
): number {
	const elapsedMs = Math.max(Date.now() - telemetry.startedAt, 1);
	return (telemetry.requestsFinished * 1000) / elapsedMs;
}

function formatRemoteDependencyDuration(durationMs: number): string {
	if (durationMs < 1000) {
		return `${durationMs}ms`;
	}
	return `${(durationMs / 1000).toFixed(1)}s`;
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
			`Create ${manifestFileName} in "${workspaceFolder.name}"?`,
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
		dependencyMode: dependencyModeRemoteOnDemand,
	});
	await notifyWorkspaceManifestUpdated(workspaceFolder);
	dismissedWorkspaceConfigPrompts.add(workspaceFolder.uri.toString());

	if (options.openManifest) {
		const document = await vscode.workspace.openTextDocument(manifestUri);
		await vscode.window.showTextDocument(document, { preview: false });
	}

	void vscode.window.showInformationMessage(
		alreadyExists
			? `${manifestFileName} already exists. Configure [dependency_store] to enable centralized ADT dependency fetches.`
			: `Created ${manifestFileName}. Configure [dependency_store] to enable centralized ADT dependency fetches.`,
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
		dependencyMode: dependencyModeRemoteOnDemand,
	});
	await notifyWorkspaceManifestUpdated(workspaceFolder);
	dismissedWorkspaceConfigPrompts.add(workspaceFolder.uri.toString());

	void vscode.window.showInformationMessage(
		`Created linked ABAP project in "${workspaceFolder.name}".`,
	);
}

async function addEditableAdtObjectToWorkspace(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
	target: EditableAdtObjectTarget,
): Promise<void> {
	if (!isSupportedEditableWorkspaceObject(objectRef)) {
		throw new Error(`Unsupported editable object type for ${objectRef.name} (${objectRef.type}).`);
	}
	if (objectRef.type.toUpperCase() === "FUGR/F" || isFunctionModuleObject(objectRef)) {
		if (target.kind !== "directory") {
			throw new Error(`Function group objects require a target directory: ${objectRef.name}.`);
		}
		await addEditableFunctionGroupToWorkspace(context, workspaceFolder, objectRef, target.directoryPath);
		return;
	}
	if (!isCustomEditableObjectName(objectRef.name)) {
		throw new Error(`Only customer objects with Z/Y prefixes or customer namespaces can be added to the workspace: ${objectRef.name}.`);
	}

	if (target.kind !== "file") {
		throw new Error(`Editable object requires a target ABAP file: ${objectRef.name}.`);
	}
	const filePath = target.filePath;
	await fs.promises.mkdir(path.dirname(filePath), { recursive: true });

	let source: string;
	let fileExisted = false;
	if (await fileExists(filePath)) {
		source = await fs.promises.readFile(filePath, "utf8");
		fileExisted = true;
	} else {
		const connection = await getSapConnectionConfig(context, workspaceFolder);
		if (!connection) {
			return;
		}

		const adtClient = createAdtClient(connection);
		source = await adtClient.fetchObjectSource(objectRef.uri);
		await fs.promises.writeFile(filePath, source, "utf8");
	}

	await ensureWorkspaceManifest(workspaceFolder, {
		dependencyMode: dependencyModeRemoteOnDemand,
	});
	// Server only loads abapls.toml at workspace init or on this notification;
	// without it, remote-on-demand resolution stays disabled until restart.
	await notifyWorkspaceManifestUpdated(workspaceFolder);

	const document = await vscode.workspace.openTextDocument(vscode.Uri.file(filePath));
	await vscode.window.showTextDocument(document, { preview: false });

	if (!fileExisted) {
		void vscode.window.showInformationMessage(
			`Added ${objectRef.name} to ${workspaceRelativePath(workspaceFolder, filePath)}.`,
		);
	}
}

async function addEditableFunctionGroupToWorkspace(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
	targetDirectoryPath: string,
): Promise<void> {
	const functionGroupRef = editableFunctionGroupObjectRef(objectRef);
	if (!isCustomEditableObjectName(functionGroupRef.name)) {
		throw new Error(
			`Only customer objects with Z/Y prefixes or customer namespaces can be added to the workspace: ${functionGroupRef.name}.`,
		);
	}

	const connection = await getSapConnectionConfig(context, workspaceFolder);
	if (!connection) {
		return;
	}

	const adtClient = createAdtClient(connection);
	const groupChildren = await adtClient.listFunctionGroupChildren(functionGroupRef.name);
	const layout = editableFunctionGroupLayout(targetDirectoryPath, functionGroupRef, groupChildren, objectRef);
	await fs.promises.mkdir(layout.baseDir, { recursive: true });

	const createdFiles: string[] = [];
	if (!(await fileExists(layout.rootFilePath))) {
		const source = await adtClient.fetchObjectSource(functionGroupRef.uri);
		await fs.promises.writeFile(layout.rootFilePath, source, "utf8");
		createdFiles.push(layout.rootFilePath);
	}
	for (const member of layout.members) {
		if (await fileExists(member.filePath)) {
			continue;
		}

		await fs.promises.mkdir(path.dirname(member.filePath), { recursive: true });
		const source = await adtClient.fetchObjectSource(member.objectRef.uri);
		await fs.promises.writeFile(member.filePath, source, "utf8");
		createdFiles.push(member.filePath);
	}

	await ensureWorkspaceManifest(workspaceFolder, {
		dependencyMode: dependencyModeRemoteOnDemand,
	});
	await notifyWorkspaceManifestUpdated(workspaceFolder);

	const openPath = layout.openMember?.filePath ?? layout.rootFilePath;
	const document = await vscode.workspace.openTextDocument(vscode.Uri.file(openPath));
	await vscode.window.showTextDocument(document, { preview: false });

	if (createdFiles.length > 0) {
		void vscode.window.showInformationMessage(
			`Added function group ${functionGroupRef.name} to ${workspaceRelativePath(workspaceFolder, layout.baseDir)}.`,
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
	baseDir: string,
	functionGroupRef: AdtObjectRef,
	groupChildren: readonly AdtRepositoryChild[],
	selectedObjectRef: AdtObjectRef,
): {
	baseDir: string;
	rootFilePath: string;
	openMember?: EditableFunctionGroupMember;
	members: EditableFunctionGroupMember[];
} {
	const encodedGroupName = encodeURIComponent(functionGroupRef.name.trim().toUpperCase());
	const rootFilePath = path.join(baseDir, `${encodedGroupName}.abap`);

	const includeChildren = groupChildren
		.filter((child) => child.objectRef.type.toUpperCase() === "FUGR/I")
		.sort((left, right) => left.objectRef.name.localeCompare(right.objectRef.name));
	const functionModuleChildren = groupChildren
		.filter((child) => child.objectRef.type.toUpperCase() === "FUGR/FF")
		.sort((left, right) => left.objectRef.name.localeCompare(right.objectRef.name));

	const members: EditableFunctionGroupMember[] = [
		...includeChildren.map((child) => ({
			objectRef: child.objectRef,
			filePath: path.join(
				baseDir,
				"includes",
				`${encodeURIComponent(normalizedAdtObjectName(child.objectRef.name))}.abap`,
			),
		})),
		...functionModuleChildren.map((child) => ({
			objectRef: child.objectRef,
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
		openMember: members.find((member) =>
			member.objectRef.uri === selectedObjectRef.uri ||
			normalizedAdtObjectName(member.objectRef.name) === normalizedAdtObjectName(selectedObjectRef.name),
		),
	};
}

interface EditableFunctionGroupMember {
	objectRef: AdtObjectRef;
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

function dependencyArtifactPayload(
	objectRef: AdtObjectRef,
	artifact: { body: string; fileExtension: "abap" | "xml" },
): DependencyArtifactPayload {
	const manifestUnit = inferManifestUnitSpec(
		objectRef,
		artifact.fileExtension === "xml" ? "dependency.xml" : "dependency.abap",
	);
	return {
		packageName: objectRef.packageName ?? "",
		objectKind: manifestUnit.kind,
		objectName: objectRef.name,
		objectUri: objectRef.uri,
		objectType: objectRef.type,
		description: objectRef.description ?? "",
		fileExtension: artifact.fileExtension,
		sourceText: artifact.body,
		fetchedAt: new Date().toISOString(),
	};
}

function workspaceConnectionCacheKey(workspaceFolder: vscode.WorkspaceFolder): string {
	const baseUrl = vscode.workspace
		.getConfiguration("abap-ls", workspaceFolder.uri)
		.get<string>("sap.baseUrl", "")
		.trim()
		.toLowerCase();
	return baseUrl || "default";
}

export function shouldRetryNegativeRemoteDependencyCandidates(
	retryNegativeCandidates: boolean | undefined,
): boolean {
	return retryNegativeCandidates === true;
}

async function recordNegativeRemoteDependencyCandidate(
	workspaceFolder: vscode.WorkspaceFolder,
	batchContext: RemoteDependencyBatchContext,
	candidate: RemoteDependencyCandidate,
	reason: string,
): Promise<void> {
	void workspaceFolder;
	void reason;
	negativeRemoteDependencyCache.add(remoteDependencyCacheKey(workspaceFolder, candidate));
	batchContext.negativeCandidates.push(candidate);
}

async function persistFetchedDependencyArtifact(
	workspaceFolder: vscode.WorkspaceFolder,
	batchContext: RemoteDependencyBatchContext,
	objectRef: AdtObjectRef,
	artifact: { body: string; fileExtension: "abap" | "xml" },
	sourceUris: readonly string[],
): Promise<void> {
	void workspaceFolder;
	void sourceUris;
	batchContext.centralArtifacts.push(dependencyArtifactPayload(objectRef, artifact));
}

async function resolveRemoteDependencyCandidate(
	workspaceFolder: vscode.WorkspaceFolder,
	getAdtClient: () => Promise<AdtClient | undefined>,
	candidate: RemoteDependencyCandidate,
	sourceUris: readonly string[],
	batchContext: RemoteDependencyBatchContext,
	telemetry: RemoteDependencyWaveTelemetry,
	retryNegativeCandidates: boolean,
): Promise<RemoteDependencyResolutionResult> {
	const cacheKey = remoteDependencyCacheKey(workspaceFolder, candidate);
	if (retryNegativeCandidates) {
		negativeRemoteDependencyCache.delete(cacheKey);
	}

	const existing = pendingRemoteDependencyFetches.get(cacheKey);
	if (existing) {
		const result = await existing;
		if (result.fetchedName) {
			await clearNegativeRemoteDependencyCandidate(workspaceFolder, candidate);
		}
		return result;
	}

	const pending = (async (): Promise<RemoteDependencyResolutionResult> => {
		try {
			const dependencySourceMode = await dependencySourceModeForSources(
				batchContext,
				workspaceFolder,
				sourceUris,
			);
			const localDependencyRoots = await localDependencyRootsForSources(
				batchContext,
				workspaceFolder,
				sourceUris,
			);
			const persistLocalDependency = async (): Promise<RemoteDependencyResolutionResult | undefined> => {
				const localDependency = await resolveLocalDependencyFromExport(
					workspaceFolder,
					candidate,
					sourceUris,
					localDependencyRoots,
				);
				if (!localDependency) {
					return undefined;
				}

				await persistFetchedDependencyArtifact(
					workspaceFolder,
					batchContext,
					localDependency.objectRef,
					localDependency.artifact,
					sourceUris,
				);
				await clearNegativeRemoteDependencyCandidate(workspaceFolder, candidate);
				return { candidate, fetchedName: localDependency.objectRef.name };
			};

			if (dependencySourceMode !== "adt-first") {
				const localResult = await persistLocalDependency();
				if (localResult) {
					return localResult;
				}
			}

			const hasNegativeCandidate = !retryNegativeCandidates
				&& negativeRemoteDependencyCache.has(cacheKey);
			if (hasNegativeCandidate) {
				negativeRemoteDependencyCache.add(cacheKey);
				const localResult = dependencySourceMode === "adt-first"
					? await persistLocalDependency()
					: undefined;
				return localResult ?? { candidate, failed: true };
			}

			if (dependencySourceMode === "local-only") {
				return { candidate };
			}

			const directObjectRefs = directDependencyObjectRefs(candidate.name, candidate.kind);
			if (directObjectRefs.length > 0) {
				recordRemoteDependencyDirectFetchCandidate(telemetry);
				const directFetched = await fetchResolvedRemoteDependencyObjects(
					workspaceFolder,
					getAdtClient,
					directObjectRefs,
					sourceUris,
					dependencySourceMode,
					batchContext,
					localDependencyRoots,
				);
				if (directFetched) {
					await clearNegativeRemoteDependencyCandidate(workspaceFolder, candidate);
					return { candidate, fetchedName: candidate.name };
				}
				if (!shouldSearchAfterDirectFetchFailure(candidate)) {
					await recordNegativeRemoteDependencyCandidate(
						workspaceFolder,
						batchContext,
						candidate,
						"fetch-failed",
					);
					return { candidate, failed: true };
				}
			}

			recordRemoteDependencySearchCandidate(telemetry);
			const adtClient = await getAdtClient();
			if (!adtClient) {
				const localResult = await persistLocalDependency();
				return localResult ?? { candidate };
			}

			const objects = await adtClient.searchRepositoryObjects(candidate.name, 25);
			if (hasOnlyUnsupportedExactDomainMatches(candidate.name, objects)) {
				await recordNegativeRemoteDependencyCandidate(
					workspaceFolder,
					batchContext,
					candidate,
					"exact-match-domain-only",
				);
				const localResult = dependencySourceMode === "adt-first"
					? await persistLocalDependency()
					: undefined;
				return localResult ?? { candidate, failed: true };
			}
			const objectRefs = selectDependencyObjects(candidate.name, objects, candidate.kind);
			if (objectRefs.length === 0) {
				await recordNegativeRemoteDependencyCandidate(
					workspaceFolder,
					batchContext,
					candidate,
					"no-supported-match",
				);
				const localResult = dependencySourceMode === "adt-first"
					? await persistLocalDependency()
					: undefined;
				return localResult ?? { candidate, failed: true };
			}

			const fetchedAny = await fetchResolvedRemoteDependencyObjects(
				workspaceFolder,
				getAdtClient,
				objectRefs,
				sourceUris,
				dependencySourceMode,
				batchContext,
				localDependencyRoots,
			);
			if (!fetchedAny) {
				await recordNegativeRemoteDependencyCandidate(
					workspaceFolder,
					batchContext,
					candidate,
					"fetch-failed",
				);
				return { candidate, failed: true };
			}
			await clearNegativeRemoteDependencyCandidate(workspaceFolder, candidate);
			return { candidate, fetchedName: candidate.name };
		} catch (error) {
			if (error instanceof AdtRequestCancelledError) {
				return { candidate };
			}
			await recordNegativeRemoteDependencyCandidate(
				workspaceFolder,
				batchContext,
				candidate,
				"fetch-failed",
			);
			console.warn(`ABAP LSP remote dependency lookup failed for ${candidate.name}:`, error);
			return { candidate, failed: true };
		} finally {
			pendingRemoteDependencyFetches.delete(cacheKey);
		}
	})();

	pendingRemoteDependencyFetches.set(cacheKey, pending);
	return pending;
}

function shouldSearchAfterDirectFetchFailure(candidate: RemoteDependencyCandidate): boolean {
	switch (candidate.kind.trim().toLowerCase()) {
		case "static":
		case "type":
			return true;
		default:
			return false;
	}
}

async function fetchResolvedRemoteDependencyObjects(
	workspaceFolder: vscode.WorkspaceFolder,
	getAdtClient: () => Promise<AdtClient | undefined>,
	objectRefs: readonly AdtObjectRef[],
	sourceUris: readonly string[],
	dependencySourceMode: UnitDependencySourceMode,
	batchContext: RemoteDependencyBatchContext,
	localDependencyRoots: readonly string[],
): Promise<boolean> {
	let fetchedAny = false;
	for (const objectRef of objectRefs) {
		let fetched: AdtDependencyFetchResult;
		if (dependencySourceMode === "adt-first") {
			const adtClient = await getAdtClient();
			if (!adtClient) {
				return fetchedAny;
			}

			try {
				fetched = await adtClient.fetchDependencyObject(objectRef);
			} catch (error) {
				const localExport = await findLocalDependencyExport(
					workspaceFolder,
					objectRef,
					sourceUris,
					localDependencyRoots,
				);
				if (!localExport) {
					console.warn(`ABAP LSP remote dependency fetch failed for ${objectRef.name} [${objectRef.type}]`, error);
					continue;
				}
				fetched = localExport;
			}
		} else {
			const localExport = await findLocalDependencyExport(
				workspaceFolder,
				objectRef,
				sourceUris,
				localDependencyRoots,
			);
			if (localExport) {
				fetched = localExport;
			} else {
				const adtClient = await getAdtClient();
				if (!adtClient) {
					return fetchedAny;
				}
				try {
					fetched = await adtClient.fetchDependencyObject(objectRef);
				} catch (error) {
					console.warn(`ABAP LSP remote dependency fetch failed for ${objectRef.name} [${objectRef.type}]`, error);
					continue;
				}
			}
		}
		await persistFetchedDependencyArtifact(
			workspaceFolder,
			batchContext,
			objectRef,
			fetched,
			sourceUris,
		);
		for (const sharedDependency of fetched.sharedDependencies ?? []) {
			await persistFetchedDependencyArtifact(
				workspaceFolder,
				batchContext,
				sharedDependency.objectRef,
				sharedDependency,
				sourceUris,
			);
		}
		fetchedAny = true;
	}
	return fetchedAny;
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

	const adtClient = createAdtClient(connection);
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
	return trimmed.startsWith("Z") || trimmed.startsWith("Y") || /^\/[A-Z0-9_]+\/[A-Z0-9_\/]+$/.test(trimmed);
}

async function promptForEditableAdtObjectTarget(
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
): Promise<EditableAdtObjectTarget | undefined> {
	if (!isSupportedEditableWorkspaceObject(objectRef)) {
		throw new Error(`Unsupported editable object type for ${objectRef.name} (${objectRef.type}).`);
	}
	if (objectRef.type.toUpperCase() === "FUGR/F" || isFunctionModuleObject(objectRef)) {
		const functionGroupRef = editableFunctionGroupObjectRef(objectRef);
		if (!isCustomEditableObjectName(functionGroupRef.name)) {
			throw new Error(
				`Only customer objects with Z/Y prefixes or customer namespaces can be added to the workspace: ${functionGroupRef.name}.`,
			);
		}
		const defaultDir = path.relative(
			workspaceFolder.uri.fsPath,
			path.join(
				workspaceFolder.uri.fsPath,
				"src",
				"function-groups",
				encodeURIComponent(functionGroupRef.name.trim().toUpperCase()),
			),
		);
		const directoryPath = await promptForWorkspaceRelativePath(workspaceFolder, {
			prompt: `Target folder for ${functionGroupRef.name}`,
			value: defaultDir,
			requireAbapFile: false,
		});
		return directoryPath ? { kind: "directory", directoryPath } : undefined;
	}
	if (!isCustomEditableObjectName(objectRef.name)) {
		throw new Error(`Only customer objects with Z/Y prefixes or customer namespaces can be added to the workspace: ${objectRef.name}.`);
	}

	const defaultFile = workspaceRelativePath(
		workspaceFolder,
		targetEditableWorkspaceFilePath(workspaceFolder, objectRef),
	);
	const filePath = await promptForWorkspaceRelativePath(workspaceFolder, {
		prompt: `Target file for ${objectRef.name}`,
		value: defaultFile,
		requireAbapFile: true,
	});
	return filePath ? { kind: "file", filePath } : undefined;
}

async function promptForWorkspaceRelativePath(
	workspaceFolder: vscode.WorkspaceFolder,
	options: { prompt: string; value: string; requireAbapFile: boolean },
): Promise<string | undefined> {
	const value = await vscode.window.showInputBox({
		prompt: options.prompt,
		value: options.value,
		ignoreFocusOut: true,
		validateInput: (input) =>
			validateWorkspaceTargetPath(workspaceFolder, input, options.requireAbapFile),
	});
	if (!value?.trim()) {
		return undefined;
	}
	return resolveWorkspaceTargetPath(workspaceFolder, value.trim());
}

function validateWorkspaceTargetPath(
	workspaceFolder: vscode.WorkspaceFolder,
	value: string,
	requireAbapFile: boolean,
): string | undefined {
	const trimmed = value.trim();
	if (!trimmed) {
		return requireAbapFile ? "Enter a target ABAP file path." : "Enter a target folder path.";
	}
	const targetPath = resolveWorkspaceTargetPath(workspaceFolder, trimmed);
	if (!pathIsInsideWorkspace(workspaceFolder, targetPath)) {
		return "Target must be inside the selected workspace folder.";
	}
	if (targetPath === workspaceFolder.uri.fsPath) {
		return requireAbapFile ? "Enter a file path under the workspace." : undefined;
	}
	if (requireAbapFile && path.extname(targetPath).toLowerCase() !== ".abap") {
		return "Target file must use the .abap extension.";
	}
	return undefined;
}

function resolveWorkspaceTargetPath(
	workspaceFolder: vscode.WorkspaceFolder,
	value: string,
): string {
	return path.resolve(
		workspaceFolder.uri.fsPath,
		path.isAbsolute(value) ? value : path.join(workspaceFolder.uri.fsPath, value),
	);
}

function pathIsInsideWorkspace(
	workspaceFolder: vscode.WorkspaceFolder,
	targetPath: string,
): boolean {
	const root = normalizeWorkspaceFsPath(workspaceFolder.uri.fsPath);
	const target = normalizeWorkspaceFsPath(targetPath);
	return target === root || target.startsWith(`${root}${path.sep}`);
}

function normalizeWorkspaceFsPath(value: string): string {
	const normalized = path.resolve(value);
	return process.platform === "win32" ? normalized.toLowerCase() : normalized;
}

function workspaceRelativePath(
	workspaceFolder: vscode.WorkspaceFolder,
	filePath: string,
): string {
	return path.relative(workspaceFolder.uri.fsPath, filePath) || ".";
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

function remoteDependencyCacheKey(
	workspaceFolder: vscode.WorkspaceFolder,
	candidate: RemoteDependencyCandidate,
): string {
	return `${workspaceFolder.uri.toString()}:${candidate.name.toLowerCase()}`;
}

function remoteDependencyCandidateKey(candidate: RemoteDependencyCandidate): string {
	return `${candidate.kind.toLowerCase()}:${candidate.name.toLowerCase()}`;
}

function candidateSourceUriMap(
	params: RemoteDependencyResolveParams,
	fallbackSourceUris: readonly string[],
): Map<string, string[]> {
	const byCandidate = new Map<string, Set<string>>();
	const sourceCandidates = params.sourceCandidates ?? {};

	for (const [sourceUri, candidates] of Object.entries(sourceCandidates)) {
		for (const candidate of dedupeRemoteDependencyCandidates(candidates)) {
			const key = remoteDependencyCandidateKey(candidate);
			let sourceUris = byCandidate.get(key);
			if (!sourceUris) {
				sourceUris = new Set<string>();
				byCandidate.set(key, sourceUris);
			}
			sourceUris.add(sourceUri);
		}
	}

	for (const candidate of dedupeRemoteDependencyCandidates(params.candidates)) {
		const key = remoteDependencyCandidateKey(candidate);
		if (!byCandidate.has(key)) {
			byCandidate.set(key, new Set(fallbackSourceUris));
		}
	}

	return new Map(
		[...byCandidate.entries()].map(([key, sourceUris]) => [key, [...sourceUris]]),
	);
}

function createRemoteDependencyBatchContext(): RemoteDependencyBatchContext {
	return {
		centralArtifacts: [],
		negativeCandidates: [],
		sourceUnitSidecarPathsByKey: new Map(),
		localDependencyRootsByKey: new Map(),
		dependencySourceModeByKey: new Map(),
		localRootsBySidecarPath: new Map(),
		dependencySourceModeBySidecarPath: new Map(),
	};
}

function remoteDependencySourceKey(sourceUris: readonly string[]): string {
	return sourceUris.map((uri) => uri.trim()).filter((uri) => uri.length > 0).join("\n");
}

async function flushPendingDependencyCacheUnits(
	workspaceFolder: vscode.WorkspaceFolder,
	batchContext: RemoteDependencyBatchContext,
): Promise<void> {
	if (
		batchContext.centralArtifacts.length === 0 &&
		batchContext.negativeCandidates.length === 0
	) {
		return;
	}
	await client.sendRequest("abapls/storeRemoteDependencyArtifacts", {
		workspaceUri: workspaceFolder.uri.toString(),
		connectionKey: workspaceConnectionCacheKey(workspaceFolder),
		artifacts: batchContext.centralArtifacts,
		negative: dedupeRemoteDependencyCandidates(batchContext.negativeCandidates),
	} satisfies StoreRemoteDependencyArtifactsParams);
	batchContext.centralArtifacts = [];
	batchContext.negativeCandidates = [];
}

async function clearNegativeRemoteDependencyCandidate(
	workspaceFolder: vscode.WorkspaceFolder,
	candidate: RemoteDependencyCandidate,
): Promise<void> {
	void workspaceFolder;
	negativeRemoteDependencyCache.delete(remoteDependencyCacheKey(workspaceFolder, candidate));
}

async function findLocalDependencyExport(
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
	sourceUris: readonly string[],
	localDependencyRoots: readonly string[],
): Promise<AdtDependencyFetchResult | undefined> {
	const extensions: Array<"abap" | "xml"> = isDdicDependencyObject(objectRef)
		|| isMessageClassDependencyObject(objectRef)
		? ["xml"]
		: ["abap"];
	return findLocalDependencyExportByName(
		workspaceFolder,
		sourceUris,
		objectRef.name,
		extensions,
		objectRef.packageName,
		localDependencyRoots,
	);
}

async function findLocalDependencyExportByName(
	workspaceFolder: vscode.WorkspaceFolder,
	sourceUris: readonly string[],
	objectName: string,
	extensions: Array<"abap" | "xml">,
	packageName = "",
	localDependencyRoots?: readonly string[],
): Promise<AdtDependencyFetchResult | undefined> {
	const roots = localDependencyRoots
		? [...localDependencyRoots]
		: await localDependencyRootsFromUnitSidecars(workspaceFolder, sourceUris);
	if (roots.length === 0) {
		return undefined;
	}

	const encodedName = encodeURIComponent(objectName.trim().toUpperCase());
	const encodedPackageName = encodeURIComponent(packageName.trim().toUpperCase());
	for (const extension of extensions) {
		let bestMatch: { filePath: string; fileExtension: "abap" | "xml"; score: number } | undefined;
		for (const root of roots) {
			const match = await findLocalExportFileInIndexedRoot(
				root,
				encodedName,
				encodedPackageName,
				[extension],
			);
			if (!match) {
				continue;
			}
			if (!bestMatch || match.score > bestMatch.score) {
				bestMatch = match;
			}
		}

		if (!bestMatch) {
			continue;
		}

		try {
			return {
				body: await fs.promises.readFile(bestMatch.filePath, "utf8"),
				fileExtension: bestMatch.fileExtension,
				manifestKind: "",
			};
		} catch {
			clearLocalExportIndexCache();
		}
	}

	return undefined;
}

async function resolveLocalDependencyFromExport(
	workspaceFolder: vscode.WorkspaceFolder,
	candidate: RemoteDependencyCandidate,
	sourceUris: readonly string[],
	localDependencyRoots: readonly string[],
): Promise<{ objectRef: AdtObjectRef; artifact: AdtDependencyFetchResult } | undefined> {
	const extensions = localExportExtensionsForCandidateKind(candidate.kind);
	if (extensions.length === 0) {
		return undefined;
	}

	const artifact = await findLocalDependencyExportByName(
		workspaceFolder,
		sourceUris,
		candidate.name,
		extensions,
		"",
		localDependencyRoots,
	);
	if (!artifact) {
		return undefined;
	}

	const objectRef = inferLocalExportObjectRef(artifact.body, candidate.name, candidate.kind)
		?? parseLocalDdicExportObjectRef(artifact.body, candidate.name);
	if (!objectRef) {
		return undefined;
	}

	return { objectRef, artifact };
}

function localExportExtensionsForCandidateKind(
	kind: string,
): Array<"abap" | "xml"> {
	switch (kind.trim().toLowerCase()) {
		case "include":
		case "function":
		case "static":
		case "report":
			return ["abap"];
		case "message-class":
			return ["xml"];
		case "symbol":
		case "type":
			return ["xml", "abap"];
		default:
			return [];
	}
}

async function localDependencyRootsFromUnitSidecars(
	workspaceFolder: vscode.WorkspaceFolder,
	sourceUris: readonly string[],
): Promise<string[]> {
	const roots: string[] = [];
	const seenRoots = new Set<string>();
	for (const sidecarPath of await sourceUnitSidecarPaths(workspaceFolder, sourceUris)) {
		for (const root of await readUnitSidecarLocalRoots(sidecarPath)) {
			if (seenRoots.has(root)) {
				continue;
			}
			seenRoots.add(root);
			roots.push(root);
		}
	}
	return roots;
}

async function dependencySourceModeForSources(
	batchContext: RemoteDependencyBatchContext,
	workspaceFolder: vscode.WorkspaceFolder,
	sourceUris: readonly string[],
): Promise<UnitDependencySourceMode> {
	const key = remoteDependencySourceKey(sourceUris);
	let pending = batchContext.dependencySourceModeByKey.get(key);
	if (!pending) {
		pending = (async () => {
			let sawLocalFirst = false;
			let sawAdtFirst = false;
			for (const sidecarPath of await sourceUnitSidecarPathsCached(batchContext, workspaceFolder, sourceUris)) {
				const mode = await readUnitSidecarDependencySourceModeCached(batchContext, sidecarPath);
				if (mode === "local-only") {
					return "local-only";
				}
				if (mode === "local-first") {
					sawLocalFirst = true;
					continue;
				}
				if (mode === "adt-first") {
					sawAdtFirst = true;
				}
			}
			if (sawLocalFirst) {
				return "local-first";
			}
			return sawAdtFirst ? "adt-first" : "local-first";
		})();
		batchContext.dependencySourceModeByKey.set(key, pending);
	}
	return pending;
}

async function sourceUnitSidecarPaths(
	workspaceFolder: vscode.WorkspaceFolder,
	sourceUris: readonly string[],
): Promise<string[]> {
	const sidecarPaths: string[] = [];
	const seenPaths = new Set<string>();
	for (const sourceUri of sourceUris) {
		let uri: vscode.Uri;
		try {
			uri = vscode.Uri.parse(sourceUri);
		} catch {
			continue;
		}
		if (uri.scheme !== "file" || !uri.fsPath.startsWith(workspaceFolder.uri.fsPath)) {
			continue;
		}
		const siblingSidecarPath = `${uri.fsPath}.abapls-unit.toml`;
		if (!seenPaths.has(siblingSidecarPath) && await fileExists(siblingSidecarPath)) {
			seenPaths.add(siblingSidecarPath);
			sidecarPaths.push(siblingSidecarPath);
		}
		let currentDir = path.dirname(uri.fsPath);
		while (currentDir.startsWith(workspaceFolder.uri.fsPath)) {
			const sidecarPath = path.join(currentDir, "abapls-unit.toml");
			if (!seenPaths.has(sidecarPath) && await fileExists(sidecarPath)) {
				seenPaths.add(sidecarPath);
				sidecarPaths.push(sidecarPath);
			}
			if (currentDir === workspaceFolder.uri.fsPath) {
				break;
			}
			const parentDir = path.dirname(currentDir);
			if (parentDir === currentDir) {
				break;
			}
			currentDir = parentDir;
		}
	}
	return sidecarPaths;
}

async function sourceUnitSidecarPathsCached(
	batchContext: RemoteDependencyBatchContext,
	workspaceFolder: vscode.WorkspaceFolder,
	sourceUris: readonly string[],
): Promise<string[]> {
	const key = remoteDependencySourceKey(sourceUris);
	let pending = batchContext.sourceUnitSidecarPathsByKey.get(key);
	if (!pending) {
		pending = sourceUnitSidecarPaths(workspaceFolder, sourceUris);
		batchContext.sourceUnitSidecarPathsByKey.set(key, pending);
	}
	return pending;
}

async function readUnitSidecarLocalRoots(sidecarPath: string): Promise<string[]> {
	let text: string;
	try {
		text = await fs.promises.readFile(sidecarPath, "utf8");
	} catch {
		return [];
	}
	return parseUnitSidecarLocalRoots(text, sidecarPath);
}

async function readUnitSidecarLocalRootsCached(
	batchContext: RemoteDependencyBatchContext,
	sidecarPath: string,
): Promise<string[]> {
	let pending = batchContext.localRootsBySidecarPath.get(sidecarPath);
	if (!pending) {
		pending = readUnitSidecarLocalRoots(sidecarPath);
		batchContext.localRootsBySidecarPath.set(sidecarPath, pending);
	}
	return pending;
}

async function readUnitSidecarDependencySourceModeCached(
	batchContext: RemoteDependencyBatchContext,
	sidecarPath: string,
): Promise<UnitDependencySourceMode | undefined> {
	let pending = batchContext.dependencySourceModeBySidecarPath.get(sidecarPath);
	if (!pending) {
		pending = readUnitSidecarDependencySourceMode(sidecarPath);
		batchContext.dependencySourceModeBySidecarPath.set(sidecarPath, pending);
	}
	return pending;
}

async function localDependencyRootsForSources(
	batchContext: RemoteDependencyBatchContext,
	workspaceFolder: vscode.WorkspaceFolder,
	sourceUris: readonly string[],
): Promise<string[]> {
	const key = remoteDependencySourceKey(sourceUris);
	let pending = batchContext.localDependencyRootsByKey.get(key);
	if (!pending) {
		pending = (async () => {
			const roots: string[] = [];
			const seenRoots = new Set<string>();
			for (const sidecarPath of await sourceUnitSidecarPathsCached(batchContext, workspaceFolder, sourceUris)) {
				for (const root of await readUnitSidecarLocalRootsCached(batchContext, sidecarPath)) {
					if (seenRoots.has(root)) {
						continue;
					}
					seenRoots.add(root);
					roots.push(root);
				}
			}

			for (const root of vscode.workspace
				.getConfiguration("abap-ls", workspaceFolder.uri)
				.get<string[]>("sap.localDependencyRoots", [])
				.map((value) => value.trim())
				.filter((value) => Boolean(value))) {
				if (seenRoots.has(root)) {
					continue;
				}
				seenRoots.add(root);
				roots.push(root);
			}

			return roots;
		})();
		batchContext.localDependencyRootsByKey.set(key, pending);
	}
	return pending;
}

export function parseUnitSidecarLocalRoots(text: string, sidecarPath: string): string[] {
	const localExportBlock = readUnitSidecarSection(text, "local_export");
	if (localExportBlock === undefined) {
		return [];
	}

	const rootsArray = localExportBlock.match(/^\s*roots\s*=\s*\[([\s\S]*?)\]/m)?.[1];
	if (!rootsArray) {
		return [];
	}

	const roots: string[] = [];
	for (const stringMatch of rootsArray.matchAll(/"((?:[^"\\]|\\.)*)"/g)) {
		const raw = stringMatch[1].replace(/\\"/g, "\"").replace(/\\\\/g, "\\").trim();
		if (!raw) {
			continue;
		}
		roots.push(path.isAbsolute(raw) ? raw : path.resolve(path.dirname(sidecarPath), raw));
	}
	return roots;
}

export function parseUnitSidecarDependencySourceMode(
	text: string,
): UnitDependencySourceMode | undefined {
	const dependenciesBlock = readUnitSidecarSection(text, "dependencies");
	if (dependenciesBlock === undefined) {
		return undefined;
	}

	const match = dependenciesBlock.match(/^\s*source\s*=\s*(?:"([^"]+)"|'([^']+)')\s*$/m);
	if (!match) {
		return undefined;
	}
	const normalized = (match[1] ?? match[2] ?? "").trim().toLowerCase();
	if (normalized === "local-only" || normalized === "adt-first") {
		return normalized;
	}
	if (normalized === "local-first") {
		return normalized;
	}
	return undefined;
}

function readUnitSidecarSection(text: string, sectionName: string): string | undefined {
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

async function readUnitSidecarDependencySourceMode(
	sidecarPath: string,
): Promise<UnitDependencySourceMode | undefined> {
	let text: string;
	try {
		text = await fs.promises.readFile(sidecarPath, "utf8");
	} catch {
		return undefined;
	}
	return parseUnitSidecarDependencySourceMode(text);
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
	if (!client.isRunning()) {
		return;
	}
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
