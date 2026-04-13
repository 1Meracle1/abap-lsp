import * as assert from "assert";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as vscode from "vscode";

import {
	ensureManifestUnit,
	type ManifestUnitSpec,
	workspaceManifestPath,
} from "../manifest";

suite("Manifest helpers", () => {
	test("Appends a second editable unit", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("append-second-unit");

		await ensureManifestUnit(workspaceFolder, {
			name: "ZCL_FIRST",
			kind: "global-class",
			rootFile: "src/ZCL_FIRST.abap",
			adtUri: "/sap/bc/adt/oo/classes/zcl_first",
			role: "main",
			objectName: "ZCL_FIRST",
		});
		await ensureManifestUnit(workspaceFolder, {
			name: "ZCL_SECOND",
			kind: "global-class",
			rootFile: "src/ZCL_SECOND.abap",
			adtUri: "/sap/bc/adt/oo/classes/zcl_second",
			role: "main",
			objectName: "ZCL_SECOND",
		});

		const text = await fs.promises.readFile(workspaceManifestPath(workspaceFolder), "utf8");
		assert.strictEqual((text.match(/^\[\[unit\]\]$/gm) ?? []).length, 2);
		assert.ok(text.includes('root_file = "src/ZCL_FIRST.abap"'));
		assert.ok(text.includes('root_file = "src/ZCL_SECOND.abap"'));
	});

	test("Serializes multi-file function group units", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("function-group-unit");

		await ensureManifestUnit(workspaceFolder, {
			name: "/STTP/SHF_MD",
			kind: "function-group",
			rootFile: "src/function-groups/%2FSTTP%2FSHF_MD/%2FSTTP%2FSHF_MD.abap",
			adtUri: "/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md",
			role: "main",
			objectName: "/STTP/SHF_MD",
			members: [
				{
					role: "main",
					file: "src/function-groups/%2FSTTP%2FSHF_MD/%2FSTTP%2FSHF_MD.abap",
					objectName: "/STTP/SHF_MD",
					adtUri: "/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md",
				},
				{
					role: "root",
					file: "src/function-groups/%2FSTTP%2FSHF_MD/includes/%2FSTTP%2FLSHF_MDTOP.abap",
					objectName: "/STTP/LSHF_MDTOP",
					adtUri: "/sap/bc/adt/programs/includes/%2Fsttp%2Flshf_mdtop",
				},
				{
					role: "root",
					file: "src/function-groups/%2FSTTP%2FSHF_MD/function-modules/%2FSTTP%2FMD_BPNO_STS_SHF.abap",
					objectName: "/STTP/MD_BPNO_STS_SHF",
					adtUri: "/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md/fmodules/%2Fsttp%2Fmd_bpno_sts_shf",
				},
			],
		});

		const text = await fs.promises.readFile(workspaceManifestPath(workspaceFolder), "utf8");
		assert.strictEqual((text.match(/^\[\[unit\]\]$/gm) ?? []).length, 1);
		assert.strictEqual((text.match(/^\[\[unit\.member\]\]$/gm) ?? []).length, 3);
		assert.ok(text.includes('name = "/STTP/SHF_MD"'));
		assert.ok(text.includes('file = "src/function-groups/%2FSTTP%2FSHF_MD/includes/%2FSTTP%2FLSHF_MDTOP.abap"'));
		assert.ok(text.includes('file = "src/function-groups/%2FSTTP%2FSHF_MD/function-modules/%2FSTTP%2FMD_BPNO_STS_SHF.abap"'));
	});

	test("Retargets an existing dependency unit into src", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("retarget-dependency");
		const manifestPath = workspaceManifestPath(workspaceFolder);
		await fs.promises.writeFile(
			manifestPath,
			`version = 1
connection = "default"

[resolution]
dependency_mode = "remote-on-demand"
cache_dir = ".abapls/cache"
unknown_symbol_mode = "remote"
remote_request_parallelism = 4
remote_requests_per_second = 8

[[unit]]
name = "ZCL_PROMOTE"
kind = "global-class"
root_file = ".abapls/cache/dependencies/global-class/ZCL_PROMOTE.abap"
adt_uri = "/sap/bc/adt/oo/classes/zcl_promote"

[[unit.member]]
role = "dependency"
file = ".abapls/cache/dependencies/global-class/ZCL_PROMOTE.abap"
object_name = "ZCL_PROMOTE"
adt_uri = "/sap/bc/adt/oo/classes/zcl_promote"

[[unit]]
name = "ZCL_KEEP"
kind = "global-class"
root_file = ".abapls/cache/dependencies/global-class/ZCL_KEEP.abap"
adt_uri = "/sap/bc/adt/oo/classes/zcl_keep"

[[unit.member]]
role = "dependency"
file = ".abapls/cache/dependencies/global-class/ZCL_KEEP.abap"
object_name = "ZCL_KEEP"
adt_uri = "/sap/bc/adt/oo/classes/zcl_keep"
`,
			"utf8",
		);

		const promotedUnit: ManifestUnitSpec = {
			name: "ZCL_PROMOTE",
			kind: "global-class",
			rootFile: "src/ZCL_PROMOTE.abap",
			adtUri: "/sap/bc/adt/oo/classes/zcl_promote",
			role: "main",
			objectName: "ZCL_PROMOTE",
		};
		await ensureManifestUnit(workspaceFolder, promotedUnit);

		const text = await fs.promises.readFile(manifestPath, "utf8");
		assert.strictEqual((text.match(/name = "ZCL_PROMOTE"/g) ?? []).length, 1);
		assert.ok(text.includes('root_file = "src/ZCL_PROMOTE.abap"'));
		assert.ok(text.includes('role = "main"'));
		assert.ok(text.includes('root_file = ".abapls/cache/dependencies/global-class/ZCL_KEEP.abap"'));
		assert.ok(text.includes('adt_uri = "/sap/bc/adt/oo/classes/zcl_promote"\n\n[[unit]]'));
	});

	test("Retargets a function module dependency unit into a function group workspace unit", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("retarget-function-group");
		const manifestPath = workspaceManifestPath(workspaceFolder);
		await fs.promises.writeFile(
			manifestPath,
			`version = 1
connection = "default"

[resolution]
dependency_mode = "remote-on-demand"
cache_dir = ".abapls/cache"
unknown_symbol_mode = "remote"
remote_request_parallelism = 4
remote_requests_per_second = 8

[[unit]]
name = "/STTP/MD_BPNO_STS_SHF"
kind = "function-group"
root_file = ".abapls/cache/dependencies/function-group/%2FSTTP%2FMD_BPNO_STS_SHF.abap"
adt_uri = "/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md/fmodules/%2Fsttp%2Fmd_bpno_sts_shf"

[[unit.member]]
role = "dependency"
file = ".abapls/cache/dependencies/function-group/%2FSTTP%2FMD_BPNO_STS_SHF.abap"
object_name = "/STTP/MD_BPNO_STS_SHF"
adt_uri = "/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md/fmodules/%2Fsttp%2Fmd_bpno_sts_shf"
`,
			"utf8",
		);

		await ensureManifestUnit(workspaceFolder, {
			name: "/STTP/SHF_MD",
			kind: "function-group",
			rootFile: "src/function-groups/%2FSTTP%2FSHF_MD/%2FSTTP%2FSHF_MD.abap",
			adtUri: "/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md",
			role: "main",
			objectName: "/STTP/SHF_MD",
			matchAdtUris: [
				"/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md/fmodules/%2Fsttp%2Fmd_bpno_sts_shf",
			],
			members: [
				{
					role: "main",
					file: "src/function-groups/%2FSTTP%2FSHF_MD/%2FSTTP%2FSHF_MD.abap",
					objectName: "/STTP/SHF_MD",
					adtUri: "/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md",
				},
				{
					role: "root",
					file: "src/function-groups/%2FSTTP%2FSHF_MD/function-modules/%2FSTTP%2FMD_BPNO_STS_SHF.abap",
					objectName: "/STTP/MD_BPNO_STS_SHF",
					adtUri: "/sap/bc/adt/functions/groups/%2Fsttp%2Fshf_md/fmodules/%2Fsttp%2Fmd_bpno_sts_shf",
				},
			],
		});

		const text = await fs.promises.readFile(manifestPath, "utf8");
		assert.strictEqual((text.match(/^\[\[unit\]\]$/gm) ?? []).length, 1);
		assert.ok(text.includes('name = "/STTP/SHF_MD"'));
		assert.ok(text.includes('root_file = "src/function-groups/%2FSTTP%2FSHF_MD/%2FSTTP%2FSHF_MD.abap"'));
		assert.ok(!text.includes('.abapls/cache/dependencies/function-group/%2FSTTP%2FMD_BPNO_STS_SHF.abap'));
	});

	test("Serializes concurrent manifest updates", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("concurrent-unit-updates");

		await Promise.all(
			Array.from({ length: 12 }, (_, index) =>
				ensureManifestUnit(workspaceFolder, {
					name: `ZCL_CONCURRENT_${index}`,
					kind: "global-class",
					rootFile: `src/ZCL_CONCURRENT_${index}.abap`,
					adtUri: `/sap/bc/adt/oo/classes/zcl_concurrent_${index}`,
					role: "main",
					objectName: `ZCL_CONCURRENT_${index}`,
				}),
			),
		);

		const text = await fs.promises.readFile(workspaceManifestPath(workspaceFolder), "utf8");
		assert.strictEqual((text.match(/^\[\[unit\]\]$/gm) ?? []).length, 12);
		assert.strictEqual((text.match(/^\[\[unit\.member\]\]$/gm) ?? []).length, 12);
		assert.ok(!text.includes('adt_uri = "/sap/bc/adt/oo/classes/zcl_concurrent_0"[[unit]]'));
		assert.ok(!text.includes('adt_uri = "/sap/bc/adt/oo/classes/zcl_concurrent_0"[[unit.member]]'));
	});

	test("Appends multiple local-only units without adt_uri collisions", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("local-only-units");

		await ensureManifestUnit(workspaceFolder, {
			name: "ZLOCAL_REPORT",
			kind: "report",
			rootFile: "src/ZLOCAL_REPORT.abap",
			role: "root",
			objectName: "ZLOCAL_REPORT",
		});
		await ensureManifestUnit(workspaceFolder, {
			name: "ZLOCAL_INCLUDE",
			kind: "include",
			rootFile: "src/ZLOCAL_INCLUDE.abap",
			role: "root",
			objectName: "ZLOCAL_INCLUDE",
		});

		const text = await fs.promises.readFile(workspaceManifestPath(workspaceFolder), "utf8");
		assert.strictEqual((text.match(/^\[\[unit\]\]$/gm) ?? []).length, 2);
		assert.ok(text.includes('name = "ZLOCAL_REPORT"'));
		assert.ok(text.includes('name = "ZLOCAL_INCLUDE"'));
		assert.ok(!text.includes('adt_uri = ""'));
	});

	test("Retargets a local-only unit by root file", async () => {
		const workspaceFolder = await createTempWorkspaceFolder("retarget-local-only-unit");
		const manifestPath = workspaceManifestPath(workspaceFolder);
		await fs.promises.writeFile(
			manifestPath,
			`version = 1
connection = "default"

[resolution]
dependency_mode = "local-first"
cache_dir = ".abapls/cache"
unknown_symbol_mode = "log"
remote_request_parallelism = 4
remote_requests_per_second = 8

[[unit]]
name = "ZLOCAL_REPORT"
kind = "report"
root_file = "src/ZLOCAL_REPORT.abap"

[[unit.member]]
role = "root"
file = "src/ZLOCAL_REPORT.abap"
object_name = "ZLOCAL_REPORT"
`,
			"utf8",
		);

		await ensureManifestUnit(workspaceFolder, {
			name: "ZLOCAL_REPORT_RENAMED",
			kind: "report",
			rootFile: "src/ZLOCAL_REPORT.abap",
			role: "root",
			objectName: "ZLOCAL_REPORT_RENAMED",
		});

		const text = await fs.promises.readFile(manifestPath, "utf8");
		assert.strictEqual((text.match(/^\[\[unit\]\]$/gm) ?? []).length, 1);
		assert.ok(text.includes('name = "ZLOCAL_REPORT_RENAMED"'));
		assert.ok(text.includes('object_name = "ZLOCAL_REPORT_RENAMED"'));
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
