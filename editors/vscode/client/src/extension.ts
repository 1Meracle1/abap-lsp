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
} from "vscode-languageclient/node";
import {
	AdtClient,
	AdtObjectRef,
	configureSapConnection,
	getSapConnectionConfig,
	pickBestDependencyObject,
} from "./adt";
import {
	ensureManifestDependencyUnit,
	ensureManifestUnit,
	inferManifestUnitSpec,
	targetDependencyWorkspaceFilePath,
	targetWorkspaceFilePath,
} from "./manifest";

let client: LanguageClient;
const pendingRemoteDependencyFetches = new Map<string, Promise<string | undefined>>();
const negativeRemoteDependencyCache = new Set<string>();

interface RemoteDependencyCandidate {
	name: string;
	kind: string;
}

interface RemoteDependencyResolveParams {
	workspaceUri: string;
	sourceUri: string;
	candidates: RemoteDependencyCandidate[];
}

interface RemoteDependenciesUpdatedParams {
	workspaceUri: string;
	sourceUri: string;
	fetched: string[];
}

export function activate(context: vscode.ExtensionContext) {
	// let serverModule: string;
	// const debugServerPath = process.env['__ABAP_LSP_SERVER_DEBUG'];
	// if (debugServerPath) {
	// 	serverModule = debugServerPath;
	// 	if (process.platform === 'win32' && !serverModule.endsWith('.exe')) {
	// 		serverModule += '.exe';
	// 	}
	// }
	// const serverOptions: ServerOptions = {
	// 	command: serverModule,
	// 	args: [],
	// 	options: {
	// 		cwd: path.dirname(serverModule),
	// 	},
	// 	transport: TransportKind.stdio,
	// };

	const pipePath =
		process.platform === "win32"
			? "\\\\.\\pipe\\abap-ls"
			: "/tmp/abap-ls";

	const serverOptions: ServerOptions = () => {
		return new Promise<StreamInfo>((resolve, reject) => {
			const socket = net.connect(pipePath);

			socket.on("connect", () => {
				resolve({
					writer: socket,
					reader: socket,
				});
			});

			socket.on("error", (err) => {
				reject(err);
			});
		});
	};

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
					const connection = await getSapConnectionConfig(context, workspaceFolder);
					if (!connection) {
						return;
					}

					const adtClient = new AdtClient(connection);
					const source = await adtClient.fetchObjectSource(objectRef.uri);
					const filePath = targetWorkspaceFilePath(workspaceFolder, objectRef.name);
					await fs.promises.mkdir(path.dirname(filePath), { recursive: true });
					await fs.promises.writeFile(filePath, source, "utf8");

					const relativeFile = path.relative(workspaceFolder.uri.fsPath, filePath);
					const manifestSpec = inferManifestUnitSpec(objectRef, relativeFile);
					const manifestUri = await ensureManifestUnit(workspaceFolder, manifestSpec);
					await adtClient.cacheRemoteObject(workspaceFolder, objectRef, source);

					const document = await vscode.workspace.openTextDocument(vscode.Uri.file(filePath));
					await vscode.window.showTextDocument(document, { preview: false });
					void manifestUri;
				},
			);
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

	const connection = await getSapConnectionConfig(context, workspaceFolder, { promptIfMissing: false });
	if (!connection) {
		return;
	}

	const adtClient = new AdtClient(connection);
	const fetched: string[] = [];

	for (const candidate of params.candidates) {
		const fetchedName = await resolveRemoteDependencyCandidate(
			workspaceFolder,
			adtClient,
			candidate,
		);
		if (fetchedName) {
			fetched.push(fetchedName);
		}
	}

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
			const objects = await adtClient.searchRepositoryObjects(candidate.name, 25);
			const objectRef = pickBestDependencyObject(candidate.name, objects, candidate.kind);
			if (!objectRef) {
				negativeRemoteDependencyCache.add(cacheKey);
				return undefined;
			}

			const source = await adtClient.fetchObjectSource(objectRef.uri);
			const filePath = targetDependencyWorkspaceFilePath(workspaceFolder, objectRef);
			await fs.promises.mkdir(path.dirname(filePath), { recursive: true });
			await fs.promises.writeFile(filePath, source, "utf8");
			await ensureManifestDependencyUnit(workspaceFolder, objectRef, filePath);
			await adtClient.cacheRemoteObject(workspaceFolder, objectRef, source);
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

function remoteDependencyCacheKey(
	workspaceFolder: vscode.WorkspaceFolder,
	candidate: RemoteDependencyCandidate,
): string {
	return `${workspaceFolder.uri.toString()}:${candidate.kind}:${candidate.name.toLowerCase()}`;
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