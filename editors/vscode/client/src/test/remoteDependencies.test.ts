import * as assert from "assert";

import {
	dedupeRemoteDependencyCandidates,
	mergeRemoteDependencyCandidates,
	mergeRemoteDependencyFetchPolicy,
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
});
