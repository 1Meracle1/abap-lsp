import * as assert from "assert";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";

import {
	clearLocalExportIndexCache,
	findLocalExportFileInIndexedRoot,
} from "../localExportIndex";

suite("Local export index", () => {
	test("Finds nested exports and prefers matching package paths", async () => {
		const root = await fs.promises.mkdtemp(path.join(os.tmpdir(), "abap-lsp-local-export-index-"));
		await fs.promises.mkdir(path.join(root, "packages", "OTHER", "global-class"), { recursive: true });
		await fs.promises.mkdir(path.join(root, "packages", "ZPKG", "global-class"), { recursive: true });
		await fs.promises.writeFile(
			path.join(root, "packages", "OTHER", "global-class", "ZCL_DEMO.abap"),
			"CLASS zcl_demo DEFINITION. ENDCLASS.\n",
			"utf8",
		);
		await fs.promises.writeFile(
			path.join(root, "packages", "ZPKG", "global-class", "ZCL_DEMO.abap"),
			"CLASS zcl_demo DEFINITION. ENDCLASS.\n",
			"utf8",
		);

		const match = await findLocalExportFileInIndexedRoot(
			root,
			"ZCL_DEMO",
			"ZPKG",
			["abap"],
		);
		assert.ok(match);
		assert.ok(match?.filePath.replace(/\\/g, "/").includes("/ZPKG/"), `${match?.filePath}`);

		clearLocalExportIndexCache(root);
		await fs.promises.rm(root, { recursive: true, force: true });
	});

	test("Refreshes a cached root index after a miss", async () => {
		const root = await fs.promises.mkdtemp(path.join(os.tmpdir(), "abap-lsp-local-export-refresh-"));
		await fs.promises.mkdir(path.join(root, "packages", "ME", "ddic-table"), { recursive: true });
		await fs.promises.writeFile(
			path.join(root, "packages", "ME", "ddic-table", "EKPO.xml"),
			"<table/>",
			"utf8",
		);

		assert.strictEqual(
			await findLocalExportFileInIndexedRoot(root, "EKKO", "", ["xml"]),
			undefined,
		);

		await fs.promises.writeFile(
			path.join(root, "packages", "ME", "ddic-table", "EKKO.xml"),
			"<table/>",
			"utf8",
		);

		const refreshed = await findLocalExportFileInIndexedRoot(root, "EKKO", "", ["xml"]);
		assert.ok(refreshed);
		assert.ok(refreshed?.filePath.endsWith(`${path.sep}EKKO.xml`), `${refreshed?.filePath}`);

		clearLocalExportIndexCache(root);
		await fs.promises.rm(root, { recursive: true, force: true });
	});
});
