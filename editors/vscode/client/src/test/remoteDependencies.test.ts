import * as assert from "assert";

import {
	dedupeRemoteDependencyCandidates,
	mergeRemoteDependencyCandidates,
	mergeRemoteDependencyFetchPolicy,
	RemoteDependencyScheduler,
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
			remoteRequestParallelism: 8,
			remoteRequestsPerSecond: 24,
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

	test("Derives fetch parallelism from request rate when omitted", () => {
		assert.deepStrictEqual(
			resolveRemoteDependencyFetchPolicy({
				remoteRequestsPerSecond: 6,
			}),
			{
				remoteRequestParallelism: 2,
				remoteRequestsPerSecond: 6,
			},
		);
	});

	test("Allows higher derived parallelism for high request-rate workspaces", () => {
		assert.deepStrictEqual(
			resolveRemoteDependencyFetchPolicy({
				remoteRequestsPerSecond: 240,
			}),
			{
				remoteRequestParallelism: 64,
				remoteRequestsPerSecond: 240,
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

	test("Cancels queued scheduler tasks", async () => {
		const scheduler = new RemoteDependencyScheduler();
		scheduler.updatePolicy({
			remoteRequestParallelism: 1,
			remoteRequestsPerSecond: 1,
		});

		let releaseFirstTask: (() => void) | undefined;
		const firstTask = scheduler.schedule(
			() =>
				new Promise<string>((resolve) => {
					releaseFirstTask = () => resolve("done");
				}),
		);
		const secondTask = scheduler.schedule(async () => "queued");

		scheduler.cancelAll("cancelled in test");
		releaseFirstTask?.();

		await assert.doesNotReject(() => firstTask);
		await assert.rejects(
			() => secondTask,
			(error: unknown) =>
				error instanceof Error && error.name === "AdtRequestCancelledError",
		);
	});
});
