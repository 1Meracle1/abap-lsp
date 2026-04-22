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

	test("Does not rebuild a cached root index after repeated misses", async () => {
		const root = await fs.promises.mkdtemp(path.join(os.tmpdir(), "abap-lsp-local-export-miss-"));
		await fs.promises.mkdir(path.join(root, "packages", "ME", "ddic-table"), { recursive: true });
		await fs.promises.writeFile(
			path.join(root, "packages", "ME", "ddic-table", "EKPO.xml"),
			"<table/>",
			"utf8",
		);

		const fsPromises = fs.promises as typeof fs.promises & {
			readdir: typeof fs.promises.readdir;
		};
		const originalReaddir = fsPromises.readdir;
		let readdirCalls = 0;
		fsPromises.readdir = (async (...args: Parameters<typeof originalReaddir>) => {
			readdirCalls += 1;
			return await (originalReaddir as (...innerArgs: Parameters<typeof originalReaddir>) => ReturnType<typeof originalReaddir>)(...args);
		}) as typeof originalReaddir;

		try {
			assert.strictEqual(
				await findLocalExportFileInIndexedRoot(root, "EKKO", "", ["xml"]),
				undefined,
			);
			const callsAfterFirstMiss = readdirCalls;
			assert.ok(callsAfterFirstMiss > 0, "expected an initial root scan");

			assert.strictEqual(
				await findLocalExportFileInIndexedRoot(root, "EKKO", "", ["xml"]),
				undefined,
			);
			assert.strictEqual(readdirCalls, callsAfterFirstMiss);
		} finally {
			fsPromises.readdir = originalReaddir;
			clearLocalExportIndexCache(root);
			await fs.promises.rm(root, { recursive: true, force: true });
		}
	});

	test("Shares a cold root index build across concurrent lookups", async () => {
		const root = await fs.promises.mkdtemp(path.join(os.tmpdir(), "abap-lsp-local-export-cold-"));
		await fs.promises.mkdir(path.join(root, "packages", "ME", "global-class"), { recursive: true });
		await fs.promises.writeFile(
			path.join(root, "packages", "ME", "global-class", "ZCL_DEMO.abap"),
			"CLASS zcl_demo DEFINITION. ENDCLASS.\n",
			"utf8",
		);

		const fsPromises = fs.promises as typeof fs.promises & {
			readdir: typeof fs.promises.readdir;
		};
		const originalReaddir = fsPromises.readdir;
		let readdirCalls = 0;
		fsPromises.readdir = (async (...args: Parameters<typeof originalReaddir>) => {
			readdirCalls += 1;
			await new Promise((resolve) => setTimeout(resolve, 10));
			return await (originalReaddir as (...innerArgs: Parameters<typeof originalReaddir>) => ReturnType<typeof originalReaddir>)(...args);
		}) as typeof originalReaddir;

		try {
			const [first, second, third] = await Promise.all([
				findLocalExportFileInIndexedRoot(root, "ZCL_DEMO", "", ["abap"]),
				findLocalExportFileInIndexedRoot(root, "ZCL_DEMO", "", ["abap"]),
				findLocalExportFileInIndexedRoot(root, "ZCL_DEMO", "", ["abap"]),
			]);

			assert.ok(first);
			assert.ok(second);
			assert.ok(third);
			assert.strictEqual(readdirCalls, 4);
		} finally {
			fsPromises.readdir = originalReaddir;
			clearLocalExportIndexCache(root);
			await fs.promises.rm(root, { recursive: true, force: true });
		}
	});

	test("Finds newly added exports after clearing the cached root index", async () => {
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

		assert.strictEqual(
			await findLocalExportFileInIndexedRoot(root, "EKKO", "", ["xml"]),
			undefined,
		);

		clearLocalExportIndexCache(root);
		const refreshed = await findLocalExportFileInIndexedRoot(root, "EKKO", "", ["xml"]);
		assert.ok(refreshed);
		assert.ok(refreshed?.filePath.endsWith(`${path.sep}EKKO.xml`), `${refreshed?.filePath}`);

		clearLocalExportIndexCache(root);
		await fs.promises.rm(root, { recursive: true, force: true });
	});
});
