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
});

async function createTempWorkspaceFolder(name: string): Promise<vscode.WorkspaceFolder> {
	const rootPath = await fs.promises.mkdtemp(path.join(os.tmpdir(), `abap-lsp-${name}-`));
	return {
		uri: vscode.Uri.file(rootPath),
		name,
		index: 0,
	};
}
