import * as assert from "assert";
import * as path from "path";

import {
	parseUnitSidecarDependencySourceMode,
	parseUnitSidecarLocalRoots,
	shouldRetryNegativeRemoteDependencyCandidates,
} from "../extension";

suite("Extension helpers", () => {
	test("Parses local export roots from unit sidecar single-line arrays", () => {
		const sidecarPath =
			"D:/dev/abap/prod_rep_check/src/reports/ZATTP_RS_BATCH_JOB2/abapls-unit.toml";
		const text = [
			'includes = { "ZATTP_RS_BATCH_JOB_TOP" = "ZATTP_SR_BATCH_JOB_TOP.abap" }',
			"",
			"[local_export]",
			'roots = ["D:/dev/abap/prod_rep_check/export"]',
			"",
			"[dependencies]",
			'source = "local-first"',
		].join("\n");

		assert.deepStrictEqual(parseUnitSidecarLocalRoots(text, sidecarPath), [
			"D:/dev/abap/prod_rep_check/export",
		]);
	});

	test("Resolves relative local export roots from unit sidecars", () => {
		const sidecarPath =
			"D:/dev/abap/prod_rep_check/src/reports/ZATTP_RS_BATCH_JOB2/abapls-unit.toml";
		const text = [
			"[local_export]",
			'roots = ["../../../export"]',
		].join("\n");

		assert.deepStrictEqual(parseUnitSidecarLocalRoots(text, sidecarPath), [
			path.resolve(path.dirname(sidecarPath), "../../../export"),
		]);
	});

	test("Parses dependency source mode from unit sidecars", () => {
		const text = [
			"[local_export]",
			'roots = ["D:/dev/abap/prod_rep_check/export"]',
			"",
			"[dependencies]",
			'source = "local-first"',
		].join("\n");

		assert.strictEqual(parseUnitSidecarDependencySourceMode(text), "local-first");
	});

	test("Retries negative dependency candidates only when explicitly requested", () => {
		assert.strictEqual(shouldRetryNegativeRemoteDependencyCandidates(undefined), false);
		assert.strictEqual(shouldRetryNegativeRemoteDependencyCandidates(false), false);
		assert.strictEqual(shouldRetryNegativeRemoteDependencyCandidates(true), true);
	});
});
