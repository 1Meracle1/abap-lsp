import * as assert from "assert";

import {
	computeAbapFoldingRanges,
	validateLocalWorkspaceObjectNameForKind,
} from "../extension";

suite("Extension local object validation", () => {
	test("Accepts customer global class names without ZCL prefix", () => {
		assert.strictEqual(
			validateLocalWorkspaceObjectNameForKind("zattp_cl_something", "global-class"),
			undefined,
		);
	});

	test("Accepts customer global interface names without ZIF prefix", () => {
		assert.strictEqual(
			validateLocalWorkspaceObjectNameForKind("zattp_if_something", "global-interface"),
			undefined,
		);
	});

	test("Rejects non-customer local object names", () => {
		assert.strictEqual(
			validateLocalWorkspaceObjectNameForKind("cl_demo", "global-class"),
			"Only customer objects starting with Z or Y are supported.",
		);
	});

	test("Folds IF branch only until ELSE", () => {
		const text = [
			"IF foo = abap_true.",
			"  DATA(bar) = 1.",
			"ELSE.",
			"  DATA(baz) = 2.",
			"ENDIF.",
		].join("\n");

		assert.deepStrictEqual(computeAbapFoldingRanges(text), [
			{ start: 0, end: 1, kind: undefined },
			{ start: 2, end: 3, kind: undefined },
		]);
	});

	test("Folds IF and ELSEIF branches to the next branch", () => {
		const text = [
			"IF foo = 1.",
			"  WRITE / 'one'.",
			"ELSEIF foo = 2.",
			"  WRITE / 'two'.",
			"ELSE.",
			"  WRITE / 'other'.",
			"ENDIF.",
		].join("\n");

		assert.deepStrictEqual(computeAbapFoldingRanges(text), [
			{ start: 0, end: 1, kind: undefined },
			{ start: 2, end: 3, kind: undefined },
			{ start: 4, end: 5, kind: undefined },
		]);
	});

	test("Keeps nested IF ranges separate", () => {
		const text = [
			"IF outer = abap_true.",
			"  IF inner = abap_true.",
			"    WRITE / 'x'.",
			"  ELSE.",
			"    WRITE / 'y'.",
			"  ENDIF.",
			"ELSE.",
			"  WRITE / 'z'.",
			"ENDIF.",
		].join("\n");

		assert.deepStrictEqual(computeAbapFoldingRanges(text), [
			{ start: 1, end: 2, kind: undefined },
			{ start: 3, end: 4, kind: undefined },
			{ start: 0, end: 5, kind: undefined },
			{ start: 6, end: 7, kind: undefined },
		]);
	});

	test("Folds CASE to ENDCASE and WHEN branches to the next branch", () => {
		const text = [
			"CASE foo.",
			"  WHEN 1.",
			"    WRITE / 'one'.",
			"  WHEN 2.",
			"    WRITE / 'two'.",
			"  ELSE.",
			"    WRITE / 'other'.",
			"ENDCASE.",
		].join("\n");

		assert.deepStrictEqual(computeAbapFoldingRanges(text), [
			{ start: 1, end: 2, kind: undefined },
			{ start: 3, end: 4, kind: undefined },
			{ start: 5, end: 6, kind: undefined },
			{ start: 0, end: 6, kind: undefined },
		]);
	});

	test("Keeps nested CASE ranges separate from outer CASE branches", () => {
		const text = [
			"CASE outer.",
			"  WHEN 1.",
			"    CASE inner.",
			"      WHEN 'A'.",
			"        WRITE / 'a'.",
			"      ELSE.",
			"        WRITE / 'b'.",
			"    ENDCASE.",
			"  ELSE.",
			"    WRITE / 'other'.",
			"ENDCASE.",
		].join("\n");

		assert.deepStrictEqual(computeAbapFoldingRanges(text), [
			{ start: 3, end: 4, kind: undefined },
			{ start: 5, end: 6, kind: undefined },
			{ start: 2, end: 6, kind: undefined },
			{ start: 1, end: 7, kind: undefined },
			{ start: 8, end: 9, kind: undefined },
			{ start: 0, end: 9, kind: undefined },
		]);
	});
});
