import * as assert from "assert";
import * as vscode from "vscode";

import {
	buildFunctionModuleDependencySource,
	buildMessageClassObjectRef,
	extractActiveTopLevelIncludeNames,
	formatDdicXml,
	hasOnlyUnsupportedExactDomainMatches,
	inferFunctionGroupUri,
	inferDdicManifestKind,
	isDdicDependencyObject,
	isMessageClassDependencyObject,
	isSupportedDependencyObject,
	isUnsupportedDomainDependencyObject,
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

	test("Builds message class dependency paths with xml extension", () => {
		const workspaceFolder = {
			uri: vscode.Uri.file("c:\\demo"),
			name: "demo",
			index: 0,
		} as vscode.WorkspaceFolder;
		const objectRef = buildMessageClassObjectRef("/sttp/int_msg");

		const filePath = targetDependencyWorkspaceFilePath(workspaceFolder, objectRef);
		const unit = inferManifestUnitSpec(
			objectRef,
			".abapls/cache/dependencies/message-class/%2FSTTP%2FINT_MSG.xml",
		);

		assert.strictEqual(isMessageClassDependencyObject(objectRef), true);
		assert.strictEqual(isSupportedDependencyObject(objectRef, "message-class"), true);
		assert.ok(filePath.endsWith("%2FSTTP%2FINT_MSG.xml"));
		assert.strictEqual(unit.kind, "message-class");
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

	test("Treats function modules as supported function dependencies", () => {
		const functionRef: AdtObjectRef = {
			uri: "/sap/bc/adt/functions/groups/svim/fmodules/view_get_data",
			type: "FUGR/FF",
			name: "VIEW_GET_DATA",
			packageName: "SVIM",
			description: "Function module",
		};

		assert.strictEqual(isSupportedDependencyObject(functionRef, "function"), true);
		assert.strictEqual(
			pickBestDependencyObject("view_get_data", [functionRef], "function")?.name,
			"VIEW_GET_DATA",
		);
	});

	test("Prefers function module hits over function group hits for function dependencies", () => {
		const functionGroupRef: AdtObjectRef = {
			uri: "/sap/bc/adt/functions/groups/svim",
			type: "FUGR/F",
			name: "SVIM",
			packageName: "SVIM",
			description: "Function group",
		};
		const functionModuleRef: AdtObjectRef = {
			uri: "/sap/bc/adt/functions/groups/svim/fmodules/view_get_data",
			type: "FUGR/FF",
			name: "VIEW_GET_DATA",
			packageName: "SVIM",
			description: "Function module",
		};

		assert.strictEqual(
			pickBestDependencyObject(
				"view_get_data",
				[functionGroupRef, functionModuleRef],
				"function",
			)?.type,
			"FUGR/FF",
		);
	});

	test("Derives function group uri from a function module object", () => {
		const functionModuleRef: AdtObjectRef = {
			uri: "/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md/fmodules/%2Fsttp%2Fmd_bpno_sts_shf",
			type: "FUGR/FF",
			name: "/STTP/MD_BPNO_STS_SHF",
			packageName: "/STTP/MD",
			description: "Function module",
		};

		assert.strictEqual(
			inferFunctionGroupUri(functionModuleRef),
			"/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md",
		);
	});

	test("Extracts active top-level includes from function group source", () => {
		const groupSource = [
			"FUNCTION-POOL /STTP/SHF_MD.",
			"  INCLUDE /STTP/LSHF_MDTOP.                  \" Global Data",
			"  INCLUDE /STTP/LSHF_MDUXX.                  \" Function Modules",
			"* INCLUDE /STTP/LSHF_MDF...                  \" Subroutines",
			"  INCLUDE /STTP/LSHF_MDPROG.",
			"",
		].join("\n");

		assert.deepStrictEqual(extractActiveTopLevelIncludeNames(groupSource), [
			"/STTP/LSHF_MDTOP",
			"/STTP/LSHF_MDUXX",
			"/STTP/LSHF_MDPROG",
		]);
	});

	test("Builds a composite dependency source for function modules", () => {
		const groupSource = [
			"FUNCTION-POOL /STTP/SHF_MD.",
			"  INCLUDE /STTP/LSHF_MDTOP.                  \" Global Data",
			"  INCLUDE /STTP/LSHF_MDUXX.                  \" Function Modules",
			"",
		].join("\n");
		const includeSources = new Map<string, string>([
			[
				"/STTP/LSHF_MDTOP",
				"DATA gv_counter TYPE i.\n",
			],
		]);
		const functionModuleSource = [
			"FUNCTION /STTP/MD_BPNO_STS_SHF.",
			"  gv_counter = gv_counter + 1.",
			"ENDFUNCTION.",
			"",
		].join("\n");

		const composite = buildFunctionModuleDependencySource(
			"/STTP/MD_BPNO_STS_SHF",
			groupSource,
			includeSources,
			functionModuleSource,
		);

		assert.ok(composite.includes("FUNCTION-POOL /STTP/SHF_MD."));
		assert.ok(composite.includes("DATA gv_counter TYPE i."));
		assert.ok(
			composite.includes(
				"* INCLUDE /STTP/LSHF_MDUXX. Omitted in dependency cache; function module source is appended below.",
			),
		);
		assert.ok(composite.includes("FUNCTION /STTP/MD_BPNO_STS_SHF."));
		assert.ok(!composite.includes('INCLUDE /STTP/LSHF_MDTOP.                  " Global Data'));
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

	test("Recognizes domain-only exact matches as permanently unsupported", () => {
		const domainRef: AdtObjectRef = {
			uri: "/sap/bc/adt/ddic/domains/boolean",
			type: "DOMA/DT",
			name: "BOOLEAN",
			packageName: "SABAPDEMOS",
			description: "Boolean domain",
		};

		assert.strictEqual(isUnsupportedDomainDependencyObject(domainRef), true);
		assert.strictEqual(hasOnlyUnsupportedExactDomainMatches("BOOLEAN", [domainRef]), true);
		assert.strictEqual(
			hasOnlyUnsupportedExactDomainMatches("BOOLEAN", [
				domainRef,
				{
					uri: "/sap/bc/adt/ddic/dataelements/boolean",
					type: "DTEL/DE",
					name: "BOOLEAN",
					packageName: "SABAPDEMOS",
					description: "Boolean data element",
				},
			]),
			false,
		);
	});

	test("Formats DDIC XML bodies before saving", () => {
		const formatted = formatDdicXml(
			'<?xml version="1.0"?><root><node attr="x"><child>value</child></node><empty/></root>',
		);

		assert.strictEqual(
			formatted,
			[
				'<?xml version="1.0"?>',
				"<root>",
				'  <node attr="x">',
				"    <child>",
				"      value",
				"    </child>",
				"  </node>",
				"  <empty/>",
				"</root>",
				"",
			].join("\n"),
		);
	});
});
