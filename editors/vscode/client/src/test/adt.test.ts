import * as assert from "assert";

import {
	buildFunctionModuleDependencySource,
	directDependencyObjectRefs,
	buildMessageClassObjectRef,
	extractActiveTopLevelIncludeNames,
	formatDdicXml,
	hasOnlyUnsupportedExactDomainMatches,
	inferFunctionGroupUri,
	inferLocalExportObjectRef,
	inferDdicManifestKind,
	isDdicDependencyObject,
	isMessageClassDependencyObject,
	isSupportedDependencyObject,
	isUnsupportedDomainDependencyObject,
	parseLocalDdicExportObjectRef,
	parseDotenvContents,
	pickBestDependencyObject,
	selectDependencyObjects,
	resolveSapConnectionDefaults,
	SESSION_BOOTSTRAP_ACCEPT,
	type AdtObjectRef,
} from "../adt";
import { inferManifestUnitSpec } from "../manifest";

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
		const objectRef: AdtObjectRef = {
			uri: "/sap/bc/adt/ddic/structures/zstruct",
			type: "TABL/DS",
			name: "ZSTRUCT",
			packageName: "ZPKG",
			description: "Demo structure",
		};

		const unit = inferManifestUnitSpec(objectRef, "central/ZSTRUCT.xml");
		assert.strictEqual(unit.kind, "ddic-structure");
	});

	test("Parses object metadata from local DDIC exports", () => {
		const objectRef = parseLocalDdicExportObjectRef(
			[
				'<?xml version="1.0" encoding="utf-8"?>',
				'<abapsource:elementInfo adtcore:uri="/sap/bc/adt/vit/wb/object_type/tabldt/object_name/ZATTP_RS_LEG_CTR" adtcore:type="TABL/DT" adtcore:name="zattp_rs_leg_ctr" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">',
				"</abapsource:elementInfo>",
			].join(""),
			"zattp_rs_leg_ctr",
		);

		assert.ok(objectRef);
		assert.strictEqual(objectRef?.type, "TABL/DT");
		assert.strictEqual(objectRef?.name, "ZATTP_RS_LEG_CTR");
		assert.strictEqual(
			objectRef?.uri,
			"/sap/bc/adt/vit/wb/object_type/tabldt/object_name/ZATTP_RS_LEG_CTR",
		);
	});

	test("Infers global classes from local ABAP exports", () => {
		const objectRef = inferLocalExportObjectRef(
			[
				"class zattp_cl_rs_rule_proc definition public final create public.",
				"public section.",
				"endclass.",
			].join("\n"),
			"zattp_cl_rs_rule_proc",
			"static",
		);

		assert.ok(objectRef);
		assert.strictEqual(objectRef?.type, "CLAS/OC");
		assert.strictEqual(objectRef?.name, "ZATTP_CL_RS_RULE_PROC");
		assert.strictEqual(
			inferManifestUnitSpec(objectRef!, "central/ZATTP_CL_RS_RULE_PROC.abap").kind,
			"global-class",
		);
	});

	test("Infers interfaces from local type exports when source structure is unavailable", () => {
		const objectRef = inferLocalExportObjectRef(
			"* generated export header",
			"zattp_if_reporting",
			"type",
		);

		assert.ok(objectRef);
		assert.strictEqual(objectRef?.type, "INTF/OI");
		assert.strictEqual(objectRef?.name, "ZATTP_IF_REPORTING");
	});

	test("Parses dotenv connection values", () => {
		const dotenv = parseDotenvContents([
			"# comment",
			"ABAP_ADT_URL=https://sap.example.com/sap/bc/adt",
			"ABAP_ADT_USER=\"DEMO_USER\"",
			"ABAP_ADT_PASSWORD='secret value'",
			"SAPBASE_URL=https://ignored.example.com # inline comment",
		].join("\n"));

		assert.strictEqual(dotenv.get("ABAP_ADT_URL"), "https://sap.example.com/sap/bc/adt");
		assert.strictEqual(dotenv.get("ABAP_ADT_USER"), "DEMO_USER");
		assert.strictEqual(dotenv.get("ABAP_ADT_PASSWORD"), "secret value");
		assert.strictEqual(dotenv.get("SAPBASE_URL"), "https://ignored.example.com");
	});

	test("Session bootstrap accepts atom feeds and xml", () => {
		assert.ok(SESSION_BOOTSTRAP_ACCEPT.includes("application/atom+xml;type=feed"));
		assert.ok(SESSION_BOOTSTRAP_ACCEPT.includes("application/xml"));
	});

	test("Resolves SAP connection defaults from env before dotenv", () => {
		const dotenv = new Map<string, string>([
			["ABAP_ADT_URL", "https://dotenv.example.com/sap/bc/adt"],
			["ABAP_ADT_USER", "DOTENV_USER"],
			["ABAP_ADT_PASSWORD", "dotenv-secret"],
		]);

		const defaults = resolveSapConnectionDefaults(
			{
				ABAP_ADT_BASE_URL: "https://env.example.com/sap/bc/adt",
				SAPUSER: "ENV_USER",
			},
			dotenv,
		);

		assert.strictEqual(defaults.baseUrl, "https://env.example.com/sap/bc/adt");
		assert.strictEqual(defaults.username, "ENV_USER");
		assert.strictEqual(defaults.password, "dotenv-secret");
	});

	test("Builds message class dependency paths with xml extension", () => {
		const objectRef = buildMessageClassObjectRef("/sttp/int_msg");

		const unit = inferManifestUnitSpec(
			objectRef,
			"central/%2FSTTP%2FINT_MSG.xml",
		);

		assert.strictEqual(isMessageClassDependencyObject(objectRef), true);
		assert.strictEqual(isSupportedDependencyObject(objectRef, "message-class"), true);
		assert.strictEqual(unit.kind, "message-class");
	});

	test("Builds direct fetch refs for reports and includes", () => {
		assert.deepStrictEqual(
			directDependencyObjectRefs("rsnast00", "report").map((objectRef) => objectRef.uri),
			["/sap/bc/adt/programs/programs/RSNAST00"],
		);
		assert.deepStrictEqual(
			directDependencyObjectRefs("/sttp/linclude", "include").map((objectRef) => objectRef.uri),
			["/sap/bc/adt/programs/includes/%2FSTTP%2FLINCLUDE"],
		);
	});

	test("Builds direct fetch refs for class and interface-shaped dependencies", () => {
		assert.deepStrictEqual(
			directDependencyObjectRefs("cl_abap_typedescr", "type").map((objectRef) => objectRef.type),
			["CLAS/OC"],
		);
		assert.deepStrictEqual(
			directDependencyObjectRefs("/sttp/if_demo", "type").map((objectRef) => objectRef.type),
			["INTF/OI"],
		);
		assert.deepStrictEqual(
			directDependencyObjectRefs("zfactory", "static").map((objectRef) => objectRef.type),
			["CLAS/OC", "INTF/OI"],
		);
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

	test("Selects all supported exact dependency matches before falling back to one object", () => {
		const selected = selectDependencyObjects(
			"EKKO",
			[
				{
					uri: "/sap/bc/adt/ddic/dbtables/ekko",
					type: "TABL/DT",
					name: "EKKO",
					packageName: "SABAP",
					description: "Purchasing document header",
				},
				{
					uri: "/sap/bc/adt/functions/groups/mm06e0/fmodules/ekko",
					type: "FUGR/FF",
					name: "EKKO",
					packageName: "MM06E0",
					description: "Function module",
				},
			],
			"type",
		);

		assert.strictEqual(selected.length, 2);
	});

	test("Treats reports as supported report dependencies", () => {
		const reportRef: AdtObjectRef = {
			uri: "/sap/bc/adt/programs/programs/rsnast00",
			type: "PROG/P",
			name: "RSNAST00",
			packageName: "VN",
			description: "Report",
		};

		assert.strictEqual(isSupportedDependencyObject(reportRef, "report"), true);
		assert.strictEqual(
			selectDependencyObjects("rsnast00", [reportRef], "report")[0]?.type,
			"PROG/P",
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

	test("Builds a function module dependency source that keeps shared includes separate", () => {
		const groupSource = [
			"FUNCTION-POOL /STTP/SHF_MD.",
			"  INCLUDE /STTP/LSHF_MDTOP.                  \" Global Data",
			"  INCLUDE /STTP/LSHF_MDUXX.                  \" Function Modules",
			"",
		].join("\n");
		const functionModuleSource = [
			"FUNCTION /STTP/MD_BPNO_STS_SHF.",
			"  gv_counter = gv_counter + 1.",
			"ENDFUNCTION.",
			"",
		].join("\n");

		const composite = buildFunctionModuleDependencySource(
			groupSource,
			functionModuleSource,
		);

		assert.ok(composite.includes("FUNCTION-POOL /STTP/SHF_MD."));
		assert.ok(
			composite.includes(
				"* INCLUDE /STTP/LSHF_MDUXX. Omitted in dependency cache; function module stays in its own unit.",
			),
		);
		assert.ok(composite.includes("FUNCTION /STTP/MD_BPNO_STS_SHF."));
		assert.ok(composite.includes('INCLUDE /STTP/LSHF_MDTOP.                  " Global Data'));
		assert.ok(!composite.includes("DATA gv_counter TYPE i."));
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
