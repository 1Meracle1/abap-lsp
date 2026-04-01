import * as assert from "assert";

import {
	dedupeRemoteDependencyCandidates,
	resolveRemoteDependencyFetchPolicy,
} from "../remoteDependencies";

suite("Remote dependency helpers", () => {
	test("Deduplicates candidates by normalized name", () => {
		const candidates = dedupeRemoteDependencyCandidates([
			{ name: " zcl_demo ", kind: "symbol" },
			{ name: "ZCL_DEMO", kind: "static" },
			{ name: "zcl_other", kind: "type" },
		]);

		assert.deepStrictEqual(candidates, [
			{ name: "zcl_demo", kind: "static" },
			{ name: "zcl_other", kind: "type" },
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
});
