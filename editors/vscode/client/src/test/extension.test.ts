import * as assert from "assert";

import { validateLocalWorkspaceObjectNameForKind } from "../extension";

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
});
