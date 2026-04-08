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
	AdtObjectRef,
	buildMessageClassObjectRef,
	configureSapConnection,
	getSapConnectionConfig,
	pickBestDependencyObject,
} from "./adt";
import {
	dependencyModeLocalFirst,
	ensureManifestDependencyUnit,
	ensureWorkspaceManifest,
	ensureManifestUnit,
	inferManifestUnitSpec,
	manifestFileName,
	targetDependencyWorkspaceFilePath,
	targetWorkspaceFilePath,
	unknownSymbolModeLog,
	unknownSymbolLogPath,
	unknownSymbolModeRemote,
	workspaceManifestPath,
} from "./manifest";
import {
	dedupeRemoteDependencyCandidates,
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

interface RemoteDependencyResolveParams {
	workspaceUri: string;
	sourceUri: string;
	unknownSymbolMode?: string;
	remoteRequestParallelism?: number;
	remoteRequestsPerSecond?: number;
	candidates: RemoteDependencyCandidate[];
}

interface RemoteDependenciesUpdatedParams {
	workspaceUri: string;
	sourceUri: string;
	fetched: string[];
}

interface WorkspaceManifestUpdatedParams {
	workspaceUri: string;
}

export function activate(context: vscode.ExtensionContext) {
	const serverOptions = buildServerOptions();

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [
			{ scheme: "file", language: "abap" },
			{ scheme: "untitled", language: "abap" },
		],
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

	for (const candidate of candidates) {
		if (shouldLogUnknownSymbolCandidate(candidate, unknownSymbolMode)) {
			logCandidates.push(candidate);
			continue;
		}
		fetchCandidates.push(candidate);
	}

	if (logCandidates.length > 0) {
		await appendUnknownSymbolLog(workspaceFolder, params.sourceUri, logCandidates);
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
	await vscode.window.withProgress(
		{
			location: vscode.ProgressLocation.Notification,
			title: `ABAP: fetching ${total} remote dependenc${total === 1 ? "y" : "ies"} from ADT`,
			cancellable: false,
		},
		async (progress) => {
			let completed = 0;
			const fetchedNames = await Promise.all(
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
			for (const fetchedName of fetchedNames) {
				if (fetchedName) {
					fetched.push(fetchedName);
				}
			}
		},
	);

	if (fetched.length === 0) {
		return;
	}

	const updateParams: RemoteDependenciesUpdatedParams = {
		workspaceUri: params.workspaceUri,
		sourceUri: params.sourceUri,
		fetched,
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

async function addEditableAdtObjectToWorkspace(
	context: vscode.ExtensionContext,
	workspaceFolder: vscode.WorkspaceFolder,
	objectRef: AdtObjectRef,
): Promise<void> {
	if (!isSupportedEditableWorkspaceObject(objectRef)) {
		throw new Error(`Unsupported editable object type for ${objectRef.name} (${objectRef.type}).`);
	}
	if (!isCustomEditableObjectName(objectRef.name)) {
		throw new Error(`Only custom Z* or Y* objects can be added to src: ${objectRef.name}.`);
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

async function resolveRemoteDependencyCandidate(
	workspaceFolder: vscode.WorkspaceFolder,
	adtClient: AdtClient,
	candidate: RemoteDependencyCandidate,
): Promise<string | undefined> {
	const cacheKey = remoteDependencyCacheKey(workspaceFolder, candidate);
	if (negativeRemoteDependencyCache.has(cacheKey)) {
		return undefined;
	}

	const existing = pendingRemoteDependencyFetches.get(cacheKey);
	if (existing) {
		return existing;
	}

	const pending = (async () => {
		try {
			let objectRef;
			if (candidate.kind === "message-class") {
				objectRef = buildMessageClassObjectRef(candidate.name);
			} else {
				const objects = await adtClient.searchRepositoryObjects(candidate.name, 25);
				objectRef = pickBestDependencyObject(candidate.name, objects, candidate.kind);
			}
			if (!objectRef) {
				negativeRemoteDependencyCache.add(cacheKey);
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
	return pending;
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
