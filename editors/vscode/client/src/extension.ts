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
	dependencyModeRemoteOnDemand,
	ensureWorkspaceManifest,
	manifestFileName,
	targetEditableWorkspaceFilePath,
	workspaceManifestPath,
} from "./manifest";

let client: LanguageClient;
let clientLifecycle = Promise.resolve();
let workspaceAnalysisStatusBarMessage: vscode.Disposable | undefined;
const pendingWorkspaceConfigPrompts = new Set<string>();
const dismissedWorkspaceConfigPrompts = new Set<string>();
const workspaceAnalysisProgress = new Map<string, WorkspaceAnalysisProgressHandle>();

interface DependencyCacheInitializationOptions {
	dependencyCachePath?: string;
}

interface ReadDependencyDocumentResult {
	sourceText: string;
}

interface AdtObjectRef {
	uri: string;
	type: string;
	name: string;
	packageName: string;
	description: string;
}

interface AdtRepositoryChild {
	objectRef: AdtObjectRef;
	categoryTag: string;
	objectTypeLabel: string;
	expandable: boolean;
}

interface SearchRepositoryObjectsResult {
	objects: AdtObjectRef[];
}

interface MaterializeEditableAdtObjectResult {
	openedFileUri: string;
	createdFileUris: string[];
	message: string;
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

export function activate(context: vscode.ExtensionContext) {
	const serverOptions = buildServerOptions();
	const clientDocumentSelector = [
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

			await client.sendNotification("abapls/dependencyCacheRefreshRequested", {
				workspaceUri: workspaceFolder.uri.toString(),
			} satisfies WorkspaceManifestUpdatedParams);
			vscode.window.showInformationMessage("ABAP LSP dependency cache refresh requested.");
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.stopLanguageServer", async () => {
			await runClientLifecycle(async () => {
				clearProgressUi();
				await stopLanguageClient();
			});
			vscode.window.showInformationMessage("ABAP LSP language server stopped.");
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("abap-ls.restartLanguageServer", async () => {
			await runClientLifecycle(async () => {
				clearProgressUi();
				await stopLanguageClient();
				await startLanguageClient();
			});
			vscode.window.showInformationMessage("ABAP LSP language server restarted.");
		}),
	);
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
	void context;
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
	void context;
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
	void context;
	const result = await client.sendRequest<MaterializeEditableAdtObjectResult>(
		"abapls/materializeEditableAdtObject",
		{
			workspaceUri: workspaceFolder.uri.toString(),
			objectRef,
			target,
		},
	);
	const document = await vscode.workspace.openTextDocument(vscode.Uri.parse(result.openedFileUri));
	await vscode.window.showTextDocument(document, { preview: false });
	if (result.createdFileUris.length > 0 && result.message) {
		void vscode.window.showInformationMessage(result.message);
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

function isFunctionModuleObject(objectRef: AdtObjectRef): boolean {
	return objectRef.type.toUpperCase() === "FUGR/FF" ||
		objectRef.uri.toLowerCase().includes("/functions/groups/") &&
		objectRef.uri.toLowerCase().includes("/fmodules/");
}

function inferFunctionGroupUri(objectRef: AdtObjectRef): string | undefined {
	const match = objectRef.uri.match(/^(.*\/functions\/groups\/[^/]+)(?:\/fmodules\/[^/]+)?$/i);
	return match?.[1];
}

async function promptForRepositoryObject(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
): Promise<AdtObjectRef | undefined> {
	void context;
	const query = await vscode.window.showInputBox({
		prompt: "Search SAP repository objects",
		placeHolder: "ZCL_*",
		ignoreFocusOut: true,
	});
	if (!query?.trim()) {
		return undefined;
	}

	const result = await vscode.window.withProgress(
		{
			location: vscode.ProgressLocation.Notification,
			title: `Searching SAP repository for ${query.trim()}`,
		},
		() => client.sendRequest<SearchRepositoryObjectsResult>(
			"abapls/searchRepositoryObjects",
			{
				workspaceUri: workspaceFolder.uri.toString(),
				query: query.trim(),
			},
		),
	);
	const objects = result.objects;

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