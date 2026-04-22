import * as assert from "assert";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as vscode from "vscode";

import {
	dependencyCacheManifestPath,
	ensureDependencyCacheUnit,
	ensureDependencyCacheUnits,
	ensureWorkspaceManifest,
	inferManifestUnitSpec,
	targetDependencyWorkspaceFilePath,
	targetEditableWorkspaceFilePath,
	targetLocalWorkspaceFilePath,
	workspaceManifestPath,
} from "../manifest";
import type { AdtObjectRef } from "../adt";

suite("Manifest helpers", () => {
	test("Creates a settings-only workspace manifest", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("settings-only-manifest");

		await ensureWorkspaceManifest(workspaceFolder);

		const text = await fs.promises.readFile(workspaceManifestPath(workspaceFolder), "utf8");
		assert.ok(text.includes("version = 1"));
		assert.ok(text.includes("[resolution]"));
		assert.ok(!text.includes("[[unit]]"));
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

	test("Places remote function modules into their own cache units", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("function-module-cache-layout");
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

		const functionModulePath = targetDependencyWorkspaceFilePath(workspaceFolder, functionModuleRef);
		const includePath = targetDependencyWorkspaceFilePath(workspaceFolder, includeRef);
		assert.ok(functionModulePath.endsWith(path.join("packages", "SVIM", "function-module", "VIEW_GET_DATA.abap")));
		assert.ok(includePath.endsWith(path.join("packages", "SVIM", "include", "LSVIMTOP.abap")));
		assert.strictEqual(
			inferManifestUnitSpec(
				functionModuleRef,
				".abapls/cache/packages/SVIM/function-module/VIEW_GET_DATA.abap",
			).kind,
			"function-module",
		);

		await ensureDependencyCacheUnit(
			workspaceFolder,
			functionModuleRef,
			functionModulePath,
			["src/reports/ZMAIN/ZMAIN.abap"],
		);
		await ensureDependencyCacheUnit(
			workspaceFolder,
			includeRef,
			includePath,
			["src/reports/ZMAIN/ZMAIN.abap"],
		);

		const text = await fs.promises.readFile(
			dependencyCacheManifestPath(workspaceFolder, "src/reports/ZMAIN/ZMAIN.abap"),
			"utf8",
		);
		assert.ok(text.includes('kind = "function-module"'));
		assert.ok(text.includes('root_file = ".abapls/cache/packages/SVIM/function-module/VIEW_GET_DATA.abap"'));
		assert.ok(text.includes('kind = "include"'));
		assert.ok(text.includes('root_file = ".abapls/cache/packages/SVIM/include/LSVIMTOP.abap"'));
	});

	test("Keeps remote dependency units in cache-side manifests per source file", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("cache-side-dependencies");
		const dependencyPath = path.join(
			workspaceFolder.uri.fsPath,
			".abapls",
			"cache",
			"packages",
			"ZPKG",
			"global-class",
			"ZCL_REMOTE.abap",
		);
		const dependencyRef: AdtObjectRef = {
			uri: "/sap/bc/adt/oo/classes/zcl_remote",
			type: "CLAS/OC",
			name: "ZCL_REMOTE",
			packageName: "ZPKG",
			description: "Remote class",
		};

		await ensureWorkspaceManifest(workspaceFolder);
		await ensureDependencyCacheUnit(
			workspaceFolder,
			dependencyRef,
			dependencyPath,
			["src/reports/ZDEMO_REPORT/ZDEMO_REPORT.abap"],
		);

		const projectManifestText = await fs.promises.readFile(workspaceManifestPath(workspaceFolder), "utf8");
		const dependencyManifestText = await fs.promises.readFile(
			dependencyCacheManifestPath(workspaceFolder, "src/reports/ZDEMO_REPORT/ZDEMO_REPORT.abap"),
			"utf8",
		);

		assert.ok(!projectManifestText.includes("ZCL_REMOTE"));
		assert.ok(dependencyManifestText.includes('source_file = "src/reports/ZDEMO_REPORT/ZDEMO_REPORT.abap"'));
		assert.ok(dependencyManifestText.includes('name = "ZCL_REMOTE"'));
		assert.ok(dependencyManifestText.includes('package_name = "ZPKG"'));
		assert.ok(dependencyManifestText.includes('dependency_of = ['));
		assert.ok(dependencyManifestText.includes('"src/reports/ZDEMO_REPORT/ZDEMO_REPORT.abap"'));
		assert.ok(!dependencyManifestText.includes("[[unit.member]]"));
		assert.ok(!dependencyManifestText.includes("[[unit.dependency_of]]"));
		assert.ok(
			dependencyManifestText.includes(
				'root_file = ".abapls/cache/packages/ZPKG/global-class/ZCL_REMOTE.abap"',
			),
		);
	});

	test("Merges dependency source files without duplicating cache units", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("merge-cache-manifest");
		const dependencyPath = path.join(
			workspaceFolder.uri.fsPath,
			".abapls",
			"cache",
			"packages",
			"ZPKG",
			"global-class",
			"ZCL_REMOTE.abap",
		);
		const dependencyRef: AdtObjectRef = {
			uri: "/sap/bc/adt/oo/classes/zcl_remote",
			type: "CLAS/OC",
			name: "ZCL_REMOTE",
			packageName: "ZPKG",
			description: "Remote class",
		};

		await ensureDependencyCacheUnit(
			workspaceFolder,
			dependencyRef,
			dependencyPath,
			["src/reports/ZMAIN/ZMAIN.abap"],
		);
		await ensureDependencyCacheUnit(
			workspaceFolder,
			dependencyRef,
			dependencyPath,
			["src/includes/ZHELPER.abap", "src/reports/ZMAIN/ZMAIN.abap"],
		);

		const text = await fs.promises.readFile(
			dependencyCacheManifestPath(workspaceFolder, "src/reports/ZMAIN/ZMAIN.abap"),
			"utf8",
		);
		assert.strictEqual((text.match(/^\[\[unit\]\]$/gm) ?? []).length, 1);
		assert.ok(text.includes('"src/reports/ZMAIN/ZMAIN.abap"'));
		assert.ok(text.includes('"src/includes/ZHELPER.abap"'));
	});

	test("Batches dependency cache manifest updates across many units", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("bulk-cache-manifest");
		const classRef: AdtObjectRef = {
			uri: "/sap/bc/adt/oo/classes/zcl_remote_main",
			type: "CLAS/OC",
			name: "ZCL_REMOTE_MAIN",
			packageName: "ZPKG",
			description: "Main remote class",
		};
		const helperRef: AdtObjectRef = {
			uri: "/sap/bc/adt/oo/classes/zcl_remote_helper",
			type: "CLAS/OC",
			name: "ZCL_REMOTE_HELPER",
			packageName: "ZPKG",
			description: "Helper remote class",
		};

		await ensureDependencyCacheUnits(workspaceFolder, [
			{
				objectRef: classRef,
				filePath: targetDependencyWorkspaceFilePath(workspaceFolder, classRef),
				sourceFiles: [
					"src/reports/ZMAIN/ZMAIN.abap",
					"src/includes/ZHELPER.abap",
				],
			},
			{
				objectRef: helperRef,
				filePath: targetDependencyWorkspaceFilePath(workspaceFolder, helperRef),
				sourceFiles: [
					"src/reports/ZMAIN/ZMAIN.abap",
				],
			},
			{
				objectRef: classRef,
				filePath: targetDependencyWorkspaceFilePath(workspaceFolder, classRef),
				sourceFiles: [
					"src/reports/ZMAIN/ZMAIN.abap",
				],
			},
		]);

		const reportManifestText = await fs.promises.readFile(
			dependencyCacheManifestPath(workspaceFolder, "src/reports/ZMAIN/ZMAIN.abap"),
			"utf8",
		);
		const includeManifestText = await fs.promises.readFile(
			dependencyCacheManifestPath(workspaceFolder, "src/includes/ZHELPER.abap"),
			"utf8",
		);

		assert.strictEqual((reportManifestText.match(/^\[\[unit\]\]$/gm) ?? []).length, 2);
		assert.ok(reportManifestText.includes('name = "ZCL_REMOTE_MAIN"'));
		assert.ok(reportManifestText.includes('name = "ZCL_REMOTE_HELPER"'));
		assert.ok(reportManifestText.includes('"src/reports/ZMAIN/ZMAIN.abap"'));
		assert.ok(reportManifestText.includes('"src/includes/ZHELPER.abap"'));
		assert.strictEqual((includeManifestText.match(/^\[\[unit\]\]$/gm) ?? []).length, 1);
		assert.ok(includeManifestText.includes('name = "ZCL_REMOTE_MAIN"'));
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
