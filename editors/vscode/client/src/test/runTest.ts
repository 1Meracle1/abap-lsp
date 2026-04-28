import * as path from 'path';

import { runTests } from '@vscode/test-electron';

async function main() {
	try {
		const extensionDevelopmentPath = path.resolve(__dirname, '../../../');
		const extensionTestsPath = path.resolve(__dirname, './index');
		const testWorkspacePath = path.resolve(__dirname, '../../testFixture');
		const testCachePath = path.resolve(__dirname, '../../.vscode-test');

		await runTests({
			extensionDevelopmentPath,
			extensionTestsPath,
			cachePath: testCachePath,
			launchArgs: [
				testWorkspacePath,
				`--user-data-dir=${path.join(testCachePath, 'user-data')}`,
				`--extensions-dir=${path.join(testCachePath, 'extensions')}`,
			],
		});
	} catch {
		console.error('Failed to run tests');
		process.exit(1);
	}
}

main();
