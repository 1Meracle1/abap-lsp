/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import * as fs from "fs";
import { workspace, ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	const serverModule = context.asAbsolutePath(
		path.join('.\\', '..', '..', 'abap-lsp.exe')
	);
	if (!fs.existsSync(serverModule)) {
		console.error(`executable not found at: ${serverModule}`);
		return;
	}
	const serverOptions: ServerOptions = {
		command: serverModule,
		args: [],
		options: {
			cwd: path.dirname(serverModule),
		},
		transport: TransportKind.stdio,
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [
			{ scheme: 'file', language: 'abap' },
			{ scheme: 'untitled', language: 'abap' },
		],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'abap-ls',
		'ABAP Language Server',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
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