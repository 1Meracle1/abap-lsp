import * as assert from "assert";
import * as vscode from "vscode";

import {
	inferDdicManifestKind,
	isDdicDependencyObject,
	isSupportedDependencyObject,
	type AdtObjectRef,
} from "../adt";
import { inferManifestUnitSpec, targetDependencyWorkspaceFilePath } from "../manifest";

suite("ADT dependency helpers", () => {
	test("Recognizes DDIC dependency objects", () => {
		const objectRef: AdtObjectRef = {
			uri: "/sap/bc/adt/ddic/dataelements/zdemo",
			type: "DTEL/DE",
			name: "ZDEMO",
			packageName: "ZPKG",
			description: "Demo data element",
		};

		assert.strictEqual(isDdicDependencyObject(objectRef), true);
		assert.strictEqual(isSupportedDependencyObject(objectRef, "type"), true);
		assert.strictEqual(inferDdicManifestKind(objectRef), "ddic-data-element");
	});

	test("Builds DDIC dependency paths with xml extension", () => {
		const workspaceFolder = {
			uri: vscode.Uri.file("c:\\demo"),
			name: "demo",
			index: 0,
		} as vscode.WorkspaceFolder;
		const objectRef: AdtObjectRef = {
			uri: "/sap/bc/adt/ddic/structures/zstruct",
			type: "TABL/DS",
			name: "ZSTRUCT",
			packageName: "ZPKG",
			description: "Demo structure",
		};

		const filePath = targetDependencyWorkspaceFilePath(workspaceFolder, objectRef);
		const unit = inferManifestUnitSpec(objectRef, ".abapls/cache/dependencies/ddic-structure/ZSTRUCT.xml");

		assert.ok(filePath.endsWith("ZSTRUCT.xml"));
		assert.strictEqual(unit.kind, "ddic-structure");
	});
});
