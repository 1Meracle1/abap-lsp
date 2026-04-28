import * as assert from "assert";
import * as vscode from "vscode";

import { activate, extensionId, getDocUri } from "./helper";

suite("ABAP LSP extension activation", () => {
	test("activates the packaged extension id for ABAP documents", async function () {
		if (!hasConfiguredServerConnection()) {
			this.skip();
		}

		const ext = await activate(getDocUri("activation.abap"));

		assert.strictEqual(ext.id, extensionId);
		assert.strictEqual(ext.isActive, true);
	});
});

function hasConfiguredServerConnection(): boolean {
	const config = vscode.workspace.getConfiguration("abap-ls");
	return Boolean(
		process.env.__ABAP_LSP_CONNECT?.trim() ||
			process.env.__ABAP_LSP_SERVER_PATH?.trim() ||
			process.env.__ABAP_LSP_SERVER_DEBUG?.trim() ||
			config.get<string>("serverExecutable")?.trim() ||
			config.get<string>("serverTransport") === "tcp",
	);
}
