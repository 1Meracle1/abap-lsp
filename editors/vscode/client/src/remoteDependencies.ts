import * as fs from "fs";
import * as path from "path";

import {
	defaultRemoteRequestParallelism,
	defaultRemoteRequestsPerSecond,
} from "./manifest";

export interface RemoteDependencyCandidate {
	name: string;
	kind: string;
}

export interface RemoteDependencyFetchPolicy {
	remoteRequestParallelism?: number;
	remoteRequestsPerSecond?: number;
}

export interface ResolvedRemoteDependencyFetchPolicy {
	remoteRequestParallelism: number;
	remoteRequestsPerSecond: number;
}

const candidateKindPriority = new Map<string, number>([
	["message-class", 5],
	["include", 4],
	["static", 3],
	["type", 2],
	["symbol", 1],
]);

export function normalizeRemoteDependencyName(name: string): string {
	return name.trim().toLowerCase();
}

export function dedupeRemoteDependencyCandidates(
	candidates: readonly RemoteDependencyCandidate[],
): RemoteDependencyCandidate[] {
	const deduped = new Map<string, RemoteDependencyCandidate>();

	for (const candidate of candidates) {
		const normalizedName = normalizeRemoteDependencyName(candidate.name);
		if (!normalizedName) {
			continue;
		}

		const normalizedCandidate: RemoteDependencyCandidate = {
			name: normalizedName,
			kind: candidate.kind,
		};
		const existing = deduped.get(normalizedName);
		if (!existing || remoteDependencyKindPriority(normalizedCandidate.kind) > remoteDependencyKindPriority(existing.kind)) {
			deduped.set(normalizedName, normalizedCandidate);
		}
	}

	return [...deduped.values()];
}

export function mergeRemoteDependencyCandidates(
	current: readonly RemoteDependencyCandidate[],
	incoming: readonly RemoteDependencyCandidate[],
): RemoteDependencyCandidate[] {
	return dedupeRemoteDependencyCandidates([...current, ...incoming]);
}

export function resolveRemoteDependencyFetchPolicy(
	policy: RemoteDependencyFetchPolicy | undefined,
): ResolvedRemoteDependencyFetchPolicy {
	return {
		remoteRequestParallelism: clampPositiveInteger(
			policy?.remoteRequestParallelism,
			defaultRemoteRequestParallelism,
		),
		remoteRequestsPerSecond: clampPositiveInteger(
			policy?.remoteRequestsPerSecond,
			defaultRemoteRequestsPerSecond,
		),
	};
}

export function mergeRemoteDependencyFetchPolicy(
	current: RemoteDependencyFetchPolicy | undefined,
	incoming: RemoteDependencyFetchPolicy | undefined,
): RemoteDependencyFetchPolicy {
	return {
		remoteRequestParallelism: Math.max(
			current?.remoteRequestParallelism ?? 0,
			incoming?.remoteRequestParallelism ?? 0,
		) || undefined,
		remoteRequestsPerSecond: Math.max(
			current?.remoteRequestsPerSecond ?? 0,
			incoming?.remoteRequestsPerSecond ?? 0,
		) || undefined,
	};
}

export function cachedRemoteDependencyCandidatePaths(
	workspacePath: string,
	candidate: RemoteDependencyCandidate,
): string[] {
	const normalizedName = candidate.name.trim().toUpperCase();
	if (!normalizedName) {
		return [];
	}

	const encodedName = encodeURIComponent(normalizedName);
	const dependenciesRoot = path.join(workspacePath, ".abapls", "cache", "dependencies");
	const kind = candidate.kind.trim().toLowerCase();

	switch (kind) {
		case "include":
			return [
				path.join(dependenciesRoot, "include", `${encodedName}.abap`),
			];
		case "message-class":
			return [
				path.join(dependenciesRoot, "message-class", `${encodedName}.xml`),
			];
		case "symbol":
		case "static":
		case "type":
			return [
				path.join(dependenciesRoot, "global-class", `${encodedName}.abap`),
				path.join(dependenciesRoot, "global-interface", `${encodedName}.abap`),
				path.join(dependenciesRoot, "ddic-data-element", `${encodedName}.xml`),
				path.join(dependenciesRoot, "ddic-structure", `${encodedName}.xml`),
				path.join(dependenciesRoot, "ddic-table", `${encodedName}.xml`),
				path.join(dependenciesRoot, "ddic-table-type", `${encodedName}.xml`),
				path.join(dependenciesRoot, "ddic-view", `${encodedName}.xml`),
			];
		default:
			return [];
	}
}

export async function hasCachedRemoteDependencyCandidate(
	workspacePath: string,
	candidate: RemoteDependencyCandidate,
): Promise<boolean> {
	for (const candidatePath of cachedRemoteDependencyCandidatePaths(workspacePath, candidate)) {
		try {
			await fs.promises.access(candidatePath, fs.constants.F_OK);
			return true;
		} catch {
			// Keep scanning the candidate paths for a matching local cache file.
		}
	}

	return false;
}

export function negativeRemoteDependencyMarkerPath(
	workspacePath: string,
	candidate: RemoteDependencyCandidate,
): string {
	const normalizedName = candidate.name.trim().toUpperCase();
	const encodedName = encodeURIComponent(normalizedName);
	const kind = candidate.kind.trim().toLowerCase() || "unknown";
	return path.join(
		workspacePath,
		".abapls",
		"cache",
		"negative-dependencies",
		kind,
		`${encodedName}.json`,
	);
}

export async function hasNegativeRemoteDependencyCandidate(
	workspacePath: string,
	candidate: RemoteDependencyCandidate,
): Promise<boolean> {
	try {
		await fs.promises.access(negativeRemoteDependencyMarkerPath(workspacePath, candidate), fs.constants.F_OK);
		return true;
	} catch {
		return false;
	}
}

export async function markNegativeRemoteDependencyCandidate(
	workspacePath: string,
	candidate: RemoteDependencyCandidate,
	reason: string,
): Promise<void> {
	const markerPath = negativeRemoteDependencyMarkerPath(workspacePath, candidate);
	await fs.promises.mkdir(path.dirname(markerPath), { recursive: true });
	await fs.promises.writeFile(
		markerPath,
		JSON.stringify(
			{
				name: candidate.name.trim(),
				kind: candidate.kind.trim().toLowerCase(),
				reason,
				recordedAt: new Date().toISOString(),
			},
			null,
			2,
		),
		"utf8",
	);
}

export class RemoteDependencyScheduler {
	private activeCount = 0;
	private nextRequestTimestamp = 0;
	private pending: Array<() => void> = [];
	private policy = resolveRemoteDependencyFetchPolicy(undefined);

	updatePolicy(policy: RemoteDependencyFetchPolicy | undefined): void {
		this.policy = resolveRemoteDependencyFetchPolicy(policy);
		this.pump();
	}

	schedule<T>(task: () => Promise<T>): Promise<T> {
		return new Promise<T>((resolve, reject) => {
			this.pending.push(() => {
				void this.runTask(task, resolve, reject);
			});
			this.pump();
		});
	}

	async beforeRequest(): Promise<void> {
		const now = Date.now();
		const requestIntervalMs = 1000 / this.policy.remoteRequestsPerSecond;
		const scheduledAt = Math.max(now, this.nextRequestTimestamp);
		this.nextRequestTimestamp = scheduledAt + requestIntervalMs;
		await delay(scheduledAt - now);
	}

	private pump(): void {
		while (
			this.activeCount < this.policy.remoteRequestParallelism &&
			this.pending.length > 0
		) {
			const next = this.pending.shift();
			if (!next) {
				return;
			}
			this.activeCount += 1;
			next();
		}
	}

	private async runTask<T>(
		task: () => Promise<T>,
		resolve: (value: T | PromiseLike<T>) => void,
		reject: (reason?: unknown) => void,
	): Promise<void> {
		try {
			resolve(await task());
		} catch (error) {
			reject(error);
		} finally {
			this.activeCount -= 1;
			this.pump();
		}
	}
}

function remoteDependencyKindPriority(kind: string): number {
	return candidateKindPriority.get(kind) ?? 0;
}

function clampPositiveInteger(value: number | undefined, fallback: number): number {
	if (!Number.isFinite(value)) {
		return fallback;
	}

	return Math.max(Math.floor(value ?? fallback), 1);
}

function delay(ms: number): Promise<void> {
	if (ms <= 0) {
		return Promise.resolve();
	}

	return new Promise((resolve) => {
		setTimeout(resolve, ms);
	});
}
