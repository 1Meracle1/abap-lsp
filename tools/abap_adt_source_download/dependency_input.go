package main

import (
	"encoding/json"
	"fmt"
	"os"
	"sort"
	"strings"
)

type RemoteDependencyCandidate struct {
	Name string `json:"name"`
	Kind string `json:"kind"`
}

type RemoteDependencyBatch struct {
	WorkspaceRootURI      string                                 `json:"workspace_root_uri"`
	SourceURIs            []string                               `json:"source_uris"`
	SourceCandidates      map[string][]RemoteDependencyCandidate `json:"source_candidates"`
	Candidates            []RemoteDependencyCandidate            `json:"candidates"`
	EditableDocumentCount int                                    `json:"editable_document_count"`
}

type DependencyInput struct {
	Candidates       []RemoteDependencyCandidate
	SourceCandidates map[string][]RemoteDependencyCandidate
	SourceFiles      map[string][]string
}

func loadDependencyInput(path string, workspaceRoot string) (*DependencyInput, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read dependency candidates file %s: %w", path, err)
	}

	trimmed := strings.TrimSpace(strings.TrimPrefix(string(content), "\uFEFF"))
	if trimmed == "" {
		return nil, fmt.Errorf("dependency candidates file %s is empty", path)
	}

	if strings.HasPrefix(trimmed, "{") {
		return loadDependencyJSON(trimmed, workspaceRoot)
	}
	return loadDependencyText(trimmed)
}

func loadDependencyJSON(content string, workspaceRoot string) (*DependencyInput, error) {
	var batch RemoteDependencyBatch
	if err := json.Unmarshal([]byte(content), &batch); err != nil {
		return nil, fmt.Errorf("parse dependency candidate batch JSON: %w", err)
	}

	input := &DependencyInput{
		Candidates:       dedupeRemoteDependencyCandidates(batch.Candidates),
		SourceCandidates: make(map[string][]RemoteDependencyCandidate),
		SourceFiles:      make(map[string][]string),
	}

	for sourceURI, candidates := range batch.SourceCandidates {
		deduped := dedupeRemoteDependencyCandidates(candidates)
		if len(deduped) == 0 {
			continue
		}
		input.SourceCandidates[sourceURI] = deduped
		if sourceFile, err := workspaceRelativePathFromURI(workspaceRoot, sourceURI); err == nil && sourceFile != "" {
			for _, candidate := range deduped {
				key := remoteCandidateKey(candidate)
				if key == "" {
					continue
				}
				input.SourceFiles[key] = appendUniqueString(input.SourceFiles[key], sourceFile)
			}
		}
	}

	if len(input.Candidates) == 0 {
		for _, candidates := range input.SourceCandidates {
			input.Candidates = dedupeRemoteDependencyCandidates(append(input.Candidates, candidates...))
		}
	}
	if len(input.Candidates) == 0 {
		return nil, fmt.Errorf("dependency batch JSON did not contain any candidates")
	}

	return input, nil
}

func loadDependencyText(content string) (*DependencyInput, error) {
	var candidates []RemoteDependencyCandidate
	for _, rawLine := range strings.Split(content, "\n") {
		line := strings.TrimSpace(strings.TrimSuffix(rawLine, "\r"))
		if line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, ";") {
			continue
		}

		candidate := parseDependencyCandidateLine(line)
		if candidate.Name == "" {
			continue
		}
		candidates = append(candidates, candidate)
	}

	candidates = dedupeRemoteDependencyCandidates(candidates)
	if len(candidates) == 0 {
		return nil, fmt.Errorf("dependency candidates file did not contain any usable candidates")
	}

	return &DependencyInput{
		Candidates:       candidates,
		SourceCandidates: map[string][]RemoteDependencyCandidate{},
		SourceFiles:      map[string][]string{},
	}, nil
}

func parseDependencyCandidateLine(line string) RemoteDependencyCandidate {
	name := strings.TrimSpace(line)
	kind := "symbol"
	if left, right, ok := strings.Cut(line, "|"); ok {
		name = strings.TrimSpace(left)
		if parsedKind := normalizeCandidateKind(right); parsedKind != "" {
			kind = parsedKind
		}
	}

	return RemoteDependencyCandidate{
		Name: normalizeRemoteDependencyName(name),
		Kind: kind,
	}
}

func dedupeRemoteDependencyCandidates(candidates []RemoteDependencyCandidate) []RemoteDependencyCandidate {
	deduped := make(map[string]RemoteDependencyCandidate)
	for _, candidate := range candidates {
		name := normalizeRemoteDependencyName(candidate.Name)
		if name == "" {
			continue
		}
		normalized := RemoteDependencyCandidate{
			Name: name,
			Kind: normalizeCandidateKind(candidate.Kind),
		}
		if normalized.Kind == "" {
			normalized.Kind = "symbol"
		}

		key := remoteCandidateKey(normalized)
		existing, ok := deduped[name]
		if !ok || remoteDependencyKindPriority(normalized.Kind) > remoteDependencyKindPriority(existing.Kind) {
			deduped[name] = normalized
		} else if ok && key == remoteCandidateKey(existing) {
			deduped[name] = existing
		}
	}

	result := make([]RemoteDependencyCandidate, 0, len(deduped))
	for _, candidate := range deduped {
		result = append(result, candidate)
	}
	sort.Slice(result, func(i, j int) bool {
		if result[i].Name == result[j].Name {
			return result[i].Kind < result[j].Kind
		}
		return result[i].Name < result[j].Name
	})
	return result
}

func remoteDependencyKindPriority(kind string) int {
	switch normalizeCandidateKind(kind) {
	case "message-class":
		return 5
	case "include", "function", "report":
		return 4
	case "static":
		return 3
	case "type":
		return 2
	default:
		return 1
	}
}

func normalizeRemoteDependencyName(name string) string {
	return strings.ToLower(strings.TrimSpace(name))
}

func normalizeCandidateKind(kind string) string {
	return strings.ToLower(strings.TrimSpace(kind))
}

func remoteCandidateKey(candidate RemoteDependencyCandidate) string {
	name := normalizeRemoteDependencyName(candidate.Name)
	if name == "" {
		return ""
	}
	return normalizeCandidateKind(candidate.Kind) + "::" + name
}

func appendUniqueString(values []string, value string) []string {
	for _, existing := range values {
		if existing == value {
			return values
		}
	}
	return append(values, value)
}
