import * as assert from "assert";
import * as vscode from "vscode";

import {
	inferDdicManifestKind,
	isDdicDependencyObject,
	isSupportedDependencyObject,
	pickBestDependencyObject,
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

	test("Prefers a fetchable data element over unsupported exact matches", () => {
		const objects: AdtObjectRef[] = [
			{
				uri: "/sap/bc/adt/ddic/domains/boolean",
				type: "DOMA/DT",
				name: "BOOLEAN",
				packageName: "SABAPDEMOS",
				description: "Boolean domain",
			},
			{
				uri: "/sap/bc/adt/ddic/dataelements/boolean",
				type: "DTEL/DE",
				name: "BOOLEAN",
				packageName: "SABAPDEMOS",
				description: "Boolean data element",
			},
		];

		const selected = pickBestDependencyObject("BOOLEAN", objects, "type");

		assert.ok(selected);
		assert.strictEqual(selected?.type, "DTEL/DE");
		assert.strictEqual(selected?.name, "BOOLEAN");
	});

	test("Treats interfaces and exception classes as supported type dependencies", () => {
		const interfaceRef: AdtObjectRef = {
			uri: "/sap/bc/adt/oo/interfaces/if_sxml_reader",
			type: "INTF/OI",
			name: "IF_SXML_READER",
			packageName: "SXML_LIB",
			description: "SXML reader interface",
		};
		const classRef: AdtObjectRef = {
			uri: "/sap/bc/adt/oo/classes/cx_root",
			type: "CLAS/OC",
			name: "CX_ROOT",
			packageName: "SABAP_RUNTIME",
			description: "Root exception class",
		};

		assert.strictEqual(isSupportedDependencyObject(interfaceRef, "type"), true);
		assert.strictEqual(isSupportedDependencyObject(classRef, "type"), true);
		assert.strictEqual(pickBestDependencyObject("if_sxml_reader", [interfaceRef], "type")?.name, "IF_SXML_READER");
		assert.strictEqual(pickBestDependencyObject("cx_root", [classRef], "type")?.name, "CX_ROOT");
	});

	test("Returns no dependency object when ADT search only finds unsupported exact matches", () => {
		const selected = pickBestDependencyObject(
			"BOOLEAN",
			[
				{
					uri: "/sap/bc/adt/ddic/domains/boolean",
					type: "DOMA/DT",
					name: "BOOLEAN",
					packageName: "SABAPDEMOS",
					description: "Boolean domain",
				},
			],
			"type",
		);

		assert.strictEqual(selected, undefined);
	});
});
