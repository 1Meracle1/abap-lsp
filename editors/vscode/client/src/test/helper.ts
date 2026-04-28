import * as path from "path";
import * as vscode from "vscode";

export let doc: vscode.TextDocument;
export let editor: vscode.TextEditor;
export let documentEol: string;
export let platformEol: string;

export const extensionId = "1meracle1.abap-lsp";

export async function activate(docUri: vscode.Uri): Promise<vscode.Extension<unknown>> {
	const ext = vscode.extensions.getExtension(extensionId);
	if (!ext) {
		throw new Error(`Extension ${extensionId} is not available in the test host.`);
	}
	await ext.activate();
	try {
		doc = await vscode.workspace.openTextDocument(docUri);
		editor = await vscode.window.showTextDocument(doc);
		await sleep(2000);
	} catch (e) {
		console.error(e);
	}
	return ext;
}

async function sleep(ms: number) {
	return new Promise(resolve => setTimeout(resolve, ms));
}

export const getDocPath = (p: string) => {
	return path.resolve(__dirname, '../../testFixture', p);
};
export const getDocUri = (p: string) => {
	return vscode.Uri.file(getDocPath(p));
};

export async function setTestContent(content: string): Promise<boolean> {
	const all = new vscode.Range(
		doc.positionAt(0),
		doc.positionAt(doc.getText().length),
	);
	return editor.edit(eb => eb.replace(all, content));
}
