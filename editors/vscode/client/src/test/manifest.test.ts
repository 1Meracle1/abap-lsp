import * as assert from "assert";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as vscode from "vscode";

import {
	dependencyModeRemoteOnDemand,
	ensureWorkspaceManifest,
	inferManifestUnitSpec,
	manifestUsesCentralDependencyStore,
	targetEditableWorkspaceFilePath,
	targetLocalWorkspaceFilePath,
	type AdtObjectRef,
	workspaceManifestPath,
} from "../manifest";

suite("Manifest helpers", () => {
	test("Creates a settings-only workspace manifest with central-store guidance", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("settings-only-manifest");

		await ensureWorkspaceManifest(workspaceFolder);

		const text = await fs.promises.readFile(workspaceManifestPath(workspaceFolder), "utf8");
		assert.ok(text.includes("version = 1"));
		assert.ok(text.includes("[resolution]"));
		assert.ok(text.includes('dependency_mode = "remote-on-demand"'));
		assert.ok(text.includes("# [dependency_store]"));
		assert.ok(text.includes("# product_version = "));
		assert.ok(!text.includes("unknown_symbol_mode"));
	});

	test("Detects central dependency-store configuration from abapls.toml", () => {
		assert.strictEqual(
			manifestUsesCentralDependencyStore([
				"version = 1",
				"",
				"[dependency_store]",
				'product_version = "S4-2023"',
				'default_package_version = "001"',
			].join("\n")),
			true,
		);
		assert.strictEqual(
			manifestUsesCentralDependencyStore([
				"version = 1",
				"",
				"[resolution]",
				`dependency_mode = "${dependencyModeRemoteOnDemand}"`,
			].join("\n")),
			false,
		);
	});

	test("Maps local and fetched editable objects into the same src layout", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("editable-layout");
		const classRef: AdtObjectRef = {
			uri: "/sap/bc/adt/oo/classes/zcl_demo",
			type: "CLAS/OC",
			name: "ZCL_DEMO",
			packageName: "ZPKG",
			description: "Demo class",
		};
		const reportRef: AdtObjectRef = {
			uri: "/sap/bc/adt/programs/programs/zdemo_report",
			type: "PROG/P",
			name: "ZDEMO_REPORT",
			packageName: "ZPKG",
			description: "Demo report",
		};

		assert.ok(
			targetLocalWorkspaceFilePath(workspaceFolder, "global-class", "ZCL_DEMO")
				.endsWith(path.join("src", "classes", "ZCL_DEMO.abap")),
		);
		assert.ok(
			targetEditableWorkspaceFilePath(workspaceFolder, classRef)
				.endsWith(path.join("src", "classes", "ZCL_DEMO.abap")),
		);
		assert.ok(
			targetEditableWorkspaceFilePath(workspaceFolder, reportRef)
				.endsWith(path.join("src", "reports", "ZDEMO_REPORT", "ZDEMO_REPORT.abap")),
		);
	});

	test("Infers remote dependency kinds for ABAP and DDIC artifacts", () => {
		const functionModuleRef: AdtObjectRef = {
			uri: "/sap/bc/adt/functions/groups/svim/fmodules/view_get_data",
			type: "FUGR/FF",
			name: "VIEW_GET_DATA",
			packageName: "SVIM",
			description: "Function module",
		};
		const includeRef: AdtObjectRef = {
			uri: "/sap/bc/adt/programs/includes/lsvimtop",
			type: "PROG/I",
			name: "LSVIMTOP",
			packageName: "SVIM",
			description: "Include",
		};
		const ddicRef: AdtObjectRef = {
			uri: "/sap/bc/adt/ddic/structures/zstruct",
			type: "TABL/DS",
			name: "ZSTRUCT",
			packageName: "ZPKG",
			description: "Demo structure",
		};

		assert.strictEqual(
			inferManifestUnitSpec(functionModuleRef, "central/VIEW_GET_DATA.abap").kind,
			"function-module",
		);
		assert.strictEqual(
			inferManifestUnitSpec(includeRef, "central/LSVIMTOP.abap").kind,
			"include",
		);
		assert.strictEqual(
			inferManifestUnitSpec(ddicRef, "central/ZSTRUCT.xml").kind,
			"ddic-structure",
		);
	});
});

async function createTempWorkspaceFolder(name: string): Promise<vscode.WorkspaceFolder> {
	const rootPath = await fs.promises.mkdtemp(path.join(os.tmpdir(), `abap-lsp-${name}-`));
	return {
		uri: vscode.Uri.file(rootPath),
		name,
		index: 0,
	};
}
