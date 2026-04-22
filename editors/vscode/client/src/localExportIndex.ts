import * as fs from "fs";
import * as path from "path";

export type LocalExportFileExtension = "abap" | "xml";

interface LocalExportIndexEntry {
	filePath: string;
	fileExtension: LocalExportFileExtension;
	normalizedPath: string;
}

interface LocalExportIndex {
	entriesByFileName: Map<string, LocalExportIndexEntry[]>;
}

const localExportIndexCache = new Map<string, LocalExportIndex>();
const pendingLocalExportIndexBuilds = new Map<string, Promise<LocalExportIndex>>();

export function clearLocalExportIndexCache(root?: string): void {
	if (!root) {
		localExportIndexCache.clear();
		pendingLocalExportIndexBuilds.clear();
		return;
	}
	const key = normalizedLocalExportRootKey(root);
	localExportIndexCache.delete(key);
	pendingLocalExportIndexBuilds.delete(key);
}

export async function findLocalExportFileInIndexedRoot(
	root: string,
	encodedName: string,
	encodedPackageName: string,
	extensions: readonly LocalExportFileExtension[],
): Promise<{ filePath: string; fileExtension: LocalExportFileExtension; score: number } | undefined> {
	const index = await localExportIndexForRoot(root);
	return findLocalExportFileInIndex(index, encodedName, encodedPackageName, extensions);
}

async function localExportIndexForRoot(root: string): Promise<LocalExportIndex> {
	const key = normalizedLocalExportRootKey(root);
	const cached = localExportIndexCache.get(key);
	if (cached) {
		return cached;
	}

	const pending = pendingLocalExportIndexBuilds.get(key);
	if (pending) {
		return pending;
	}

	let build: Promise<LocalExportIndex>;
	build = buildLocalExportIndex(root)
		.then((index) => {
			if (pendingLocalExportIndexBuilds.get(key) === build) {
				localExportIndexCache.set(key, index);
			}
			return index;
		})
		.finally(() => {
			if (pendingLocalExportIndexBuilds.get(key) === build) {
				pendingLocalExportIndexBuilds.delete(key);
			}
		});
	pendingLocalExportIndexBuilds.set(key, build);
	return build;
}

async function buildLocalExportIndex(root: string): Promise<LocalExportIndex> {
	const entriesByFileName = new Map<string, LocalExportIndexEntry[]>();
	const normalizedRoot = path.resolve(root);
	const stack = [normalizedRoot];

	while (stack.length > 0) {
		const current = stack.pop();
		if (!current) {
			continue;
		}
		let entries: fs.Dirent[];
		try {
			entries = await fs.promises.readdir(current, { withFileTypes: true });
		} catch {
			continue;
		}
		entries.sort((left, right) => left.name.localeCompare(right.name));

		for (const entry of entries) {
			const fullPath = path.join(current, entry.name);
			if (entry.isDirectory()) {
				stack.push(fullPath);
				continue;
			}
			if (!entry.isFile()) {
				continue;
			}
			const fileExtension = localExportFileExtension(entry.name);
			if (!fileExtension) {
				continue;
			}
			const fileNameKey = entry.name.toLowerCase();
			const indexedEntry: LocalExportIndexEntry = {
				filePath: fullPath,
				fileExtension,
				normalizedPath: fullPath.replace(/\\/g, "/"),
			};
			const bucket = entriesByFileName.get(fileNameKey);
			if (bucket) {
				bucket.push(indexedEntry);
			} else {
				entriesByFileName.set(fileNameKey, [indexedEntry]);
			}
		}
	}

	return { entriesByFileName };
}

function findLocalExportFileInIndex(
	index: LocalExportIndex,
	encodedName: string,
	encodedPackageName: string,
	extensions: readonly LocalExportFileExtension[],
): { filePath: string; fileExtension: LocalExportFileExtension; score: number } | undefined {
	let bestMatch: { filePath: string; fileExtension: LocalExportFileExtension; score: number } | undefined;
	for (const extension of extensions) {
		const fileName = `${encodedName}.${extension}`.toLowerCase();
		const candidates = index.entriesByFileName.get(fileName) ?? [];
		for (const candidate of candidates) {
			const score = encodedPackageName && candidate.normalizedPath.includes(`/${encodedPackageName}/`)
				? 2
				: 1;
			if (!bestMatch || score > bestMatch.score) {
				bestMatch = {
					filePath: candidate.filePath,
					fileExtension: candidate.fileExtension,
					score,
				};
			}
		}
		if (bestMatch) {
			return bestMatch;
		}
	}
	return undefined;
}

function normalizedLocalExportRootKey(root: string): string {
	return path.resolve(root).replace(/\\/g, "/").toLowerCase();
}

function localExportFileExtension(fileName: string): LocalExportFileExtension | undefined {
	const extension = path.extname(fileName).toLowerCase();
	if (extension === ".abap") {
		return "abap";
	}
	if (extension === ".xml") {
		return "xml";
	}
	return undefined;
}
