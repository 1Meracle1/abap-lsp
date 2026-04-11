import * as assert from "assert";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";

import {
	cachedRemoteDependencyCandidatePaths,
	dedupeRemoteDependencyCandidates,
	hasCachedRemoteDependencyCandidate,
	hasNegativeRemoteDependencyCandidate,
	mergeRemoteDependencyCandidates,
	mergeRemoteDependencyFetchPolicy,
	markNegativeRemoteDependencyCandidate,
	negativeRemoteDependencyMarkerPath,
	resolveRemoteDependencyFetchPolicy,
} from "../remoteDependencies";

suite("Remote dependency helpers", () => {
	test("Deduplicates candidates by normalized name", () => {
		const candidates = dedupeRemoteDependencyCandidates([
			{ name: " zcl_demo ", kind: "symbol" },
			{ name: "ZCL_DEMO", kind: "static" },
			{ name: "zcl_other", kind: "type" },
			{ name: "/sttp/int_msg", kind: "type" },
			{ name: "/STTP/INT_MSG", kind: "message-class" },
		]);

		assert.deepStrictEqual(candidates, [
			{ name: "zcl_demo", kind: "static" },
			{ name: "zcl_other", kind: "type" },
			{ name: "/sttp/int_msg", kind: "message-class" },
		]);
	});

	test("Clamps fetch policy values", () => {
		assert.deepStrictEqual(resolveRemoteDependencyFetchPolicy(undefined), {
			remoteRequestParallelism: 4,
			remoteRequestsPerSecond: 8,
		});
		assert.deepStrictEqual(
			resolveRemoteDependencyFetchPolicy({
				remoteRequestParallelism: 0,
				remoteRequestsPerSecond: -3,
			}),
			{
				remoteRequestParallelism: 1,
				remoteRequestsPerSecond: 1,
			},
		);
	});

	test("Merges candidates with strongest kind preserved", () => {
		assert.deepStrictEqual(
			mergeRemoteDependencyCandidates(
				[
					{ name: "zcl_demo", kind: "symbol" },
					{ name: "zif_demo", kind: "type" },
				],
				[
					{ name: "ZCL_DEMO", kind: "static" },
					{ name: "zif_demo", kind: "symbol" },
				],
			),
			[
				{ name: "zcl_demo", kind: "static" },
				{ name: "zif_demo", kind: "type" },
			],
		);
	});

	test("Merges fetch policy conservatively using higher limits", () => {
		assert.deepStrictEqual(
			mergeRemoteDependencyFetchPolicy(
				{ remoteRequestParallelism: 2, remoteRequestsPerSecond: 4 },
				{ remoteRequestParallelism: 6, remoteRequestsPerSecond: 3 },
			),
			{ remoteRequestParallelism: 6, remoteRequestsPerSecond: 4 },
		);
	});

	test("Maps type candidates to supported local cache paths", () => {
		const paths = cachedRemoteDependencyCandidatePaths("c:\\demo", {
			name: "/sttp/if_demo",
			kind: "type",
		});

		assert.ok(paths.some((candidatePath) => candidatePath.endsWith("%2FSTTP%2FIF_DEMO.abap")));
		assert.ok(paths.some((candidatePath) => candidatePath.endsWith("%2FSTTP%2FIF_DEMO.xml")));
	});

	test("Maps symbol candidates to supported local cache paths", () => {
		const paths = cachedRemoteDependencyCandidatePaths("c:\\demo", {
			name: "zcl_demo",
			kind: "symbol",
		});

		assert.ok(paths.some((candidatePath) => candidatePath.endsWith("ZCL_DEMO.abap")));
		assert.ok(paths.some((candidatePath) => candidatePath.endsWith("ZCL_DEMO.xml")));
	});

	test("Maps function candidates to function-group cache paths", () => {
		const paths = cachedRemoteDependencyCandidatePaths("c:\\demo", {
			name: "/aif/file_process_data",
			kind: "function",
		});

		assert.deepStrictEqual(paths, [
			path.join(
				"c:\\demo",
				".abapls",
				"cache",
				"dependencies",
				"function-group",
				"%2FAIF%2FFILE_PROCESS_DATA.abap",
			),
		]);
	});

	test("Skips ADT fetches when a matching cached dependency file already exists", async () => {
		const workspacePath = await fs.promises.mkdtemp(path.join(os.tmpdir(), "abap-lsp-cache-hit-"));
		const cachedFile = path.join(
			workspacePath,
			".abapls",
			"cache",
			"dependencies",
			"message-class",
			"%2FSTTP%2FINT_MSG.xml",
		);
		await fs.promises.mkdir(path.dirname(cachedFile), { recursive: true });
		await fs.promises.writeFile(cachedFile, "<message-class/>", "utf8");

		await assert.doesNotReject(() =>
			hasCachedRemoteDependencyCandidate(workspacePath, {
				name: "/sttp/int_msg",
				kind: "message-class",
			}),
		);
		assert.strictEqual(
			await hasCachedRemoteDependencyCandidate(workspacePath, {
				name: "/sttp/int_msg",
				kind: "message-class",
			}),
			true,
		);

		await fs.promises.rm(workspacePath, { recursive: true, force: true });
	});

	test("Treats symbol candidates as cache hits when a matching dependency file exists", async () => {
		const workspacePath = await fs.promises.mkdtemp(path.join(os.tmpdir(), "abap-lsp-symbol-cache-hit-"));
		const cachedFile = path.join(
			workspacePath,
			".abapls",
			"cache",
			"dependencies",
			"global-class",
			"ZCL_REMOTE_DEMO.abap",
		);
		await fs.promises.mkdir(path.dirname(cachedFile), { recursive: true });
		await fs.promises.writeFile(cachedFile, "CLASS zcl_remote_demo DEFINITION.\n", "utf8");

		assert.strictEqual(
			await hasCachedRemoteDependencyCandidate(workspacePath, {
				name: "zcl_remote_demo",
				kind: "symbol",
			}),
			true,
		);

		await fs.promises.rm(workspacePath, { recursive: true, force: true });
	});

	test("Persists negative remote dependency markers across sessions", async () => {
		const workspacePath = await fs.promises.mkdtemp(path.join(os.tmpdir(), "abap-lsp-negative-hit-"));
		const candidate = {
			name: "boolean",
			kind: "type",
		};

		await markNegativeRemoteDependencyCandidate(
			workspacePath,
			candidate,
			"exact-match-domain-only",
		);

		assert.strictEqual(await hasNegativeRemoteDependencyCandidate(workspacePath, candidate), true);
		assert.ok(
			negativeRemoteDependencyMarkerPath(workspacePath, candidate).endsWith(
				path.join("negative-dependencies", "type", "BOOLEAN.json"),
			),
		);

		await fs.promises.rm(workspacePath, { recursive: true, force: true });
	});
});
