package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/url"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

type CachedRemoteObjectMetadata struct {
	URI           string `json:"uri"`
	Type          string `json:"type"`
	Name          string `json:"name"`
	PackageName   string `json:"packageName"`
	Description   string `json:"description"`
	FileExtension string `json:"fileExtension"`
	Size          int    `json:"size"`
	FetchedAt     string `json:"fetchedAt"`
}

func exportDependencies(ctx *SapContext, cfg Config) error {
	if cfg.DependencyInput == nil {
		return fmt.Errorf("dependency input is not configured")
	}

	if cfg.CleanOutput {
		if err := deleteDependencyCacheContents(cfg.OutputDir); err != nil {
			return fmt.Errorf("clean dependency cache: %w", err)
		}
	}

	var fetchedCount int
	for _, candidate := range cfg.DependencyInput.Candidates {
		if hasCachedRemoteDependencyCandidate(cfg.OutputDir, candidate) {
			if err := clearNegativeRemoteDependencyCandidate(cfg.OutputDir, candidate); err != nil {
				return err
			}
			continue
		}
		if hasNegativeRemoteDependencyCandidate(cfg.OutputDir, candidate) {
			logNegativeCandidateSkip(candidate)
			continue
		}

		objectRef, domainOnly, err := resolveDependencyObject(ctx, candidate)
		if err != nil {
			if err := markNegativeRemoteDependencyCandidate(cfg.OutputDir, candidate, "lookup-failed"); err != nil {
				return err
			}
			log.Printf("failed to resolve %s (%s): %v", candidate.Name, candidate.Kind, err)
			continue
		}
		if domainOnly {
			log.Printf("skipping %s (%s): only unsupported domain matches were found", candidate.Name, candidate.Kind)
			if err := markNegativeRemoteDependencyCandidate(cfg.OutputDir, candidate, "exact-match-domain-only"); err != nil {
				return err
			}
			continue
		}
		if objectRef == nil {
			log.Printf("skipping %s (%s): no supported ADT dependency object matched", candidate.Name, candidate.Kind)
			if err := markNegativeRemoteDependencyCandidate(cfg.OutputDir, candidate, "no-supported-match"); err != nil {
				return err
			}
			continue
		}
		log.Printf("resolved %s (%s) to %s [%s]", candidate.Name, candidate.Kind, objectRef.Name, objectRef.Type)

		fetched, err := fetchDependencyObject(ctx, *objectRef)
		if err != nil {
			if err := markNegativeRemoteDependencyCandidate(cfg.OutputDir, candidate, "fetch-failed"); err != nil {
				return err
			}
			log.Printf("failed to fetch %s [%s]: %v", objectRef.Name, objectRef.Type, err)
			continue
		}
		if err := persistFetchedDependencyArtifact(cfg.OutputDir, *objectRef, fetched, cfg.DependencyInput.SourceFiles[remoteCandidateKey(candidate)]); err != nil {
			return err
		}
		for _, shared := range fetched.SharedDependencies {
			if err := persistFetchedDependencyArtifact(cfg.OutputDir, shared.ObjectRef, AdtDependencyFetchResult{
				Body:          shared.Body,
				FileExtension: shared.FileExtension,
				ManifestKind:  shared.ManifestKind,
			}, cfg.DependencyInput.SourceFiles[remoteCandidateKey(candidate)]); err != nil {
				return err
			}
		}
		if err := clearNegativeRemoteDependencyCandidate(cfg.OutputDir, candidate); err != nil {
			return err
		}
		fetchedCount++
	}

	log.Printf("resolved %d dependency candidate(s) into %s\\.abapls\\cache", fetchedCount, cfg.OutputDir)
	return nil
}

func persistFetchedDependencyArtifact(workspaceRoot string, objectRef AdtObjectRef, artifact AdtDependencyFetchResult, sourceFiles []string) error {
	filePath := targetDependencyWorkspaceFilePath(workspaceRoot, objectRef)
	if err := os.MkdirAll(filepath.Dir(filePath), os.ModePerm); err != nil {
		return fmt.Errorf("create dependency folder %s: %w", filepath.Dir(filePath), err)
	}
	if err := os.WriteFile(filePath, []byte(artifact.Body), os.ModePerm); err != nil {
		return fmt.Errorf("write dependency file %s: %w", filePath, err)
	}
	if len(sourceFiles) > 0 {
		if err := ensureDependencyCacheUnit(workspaceRoot, objectRef, filePath, sourceFiles); err != nil {
			return err
		}
	}
	if err := writeCachedRemoteObjectMetadata(workspaceRoot, objectRef, artifact.FileExtension, len(artifact.Body)); err != nil {
		return err
	}
	return nil
}

func targetDependencyWorkspaceFilePath(workspaceRoot string, objectRef AdtObjectRef) string {
	kindDir := sanitizePathSegment(inferManifestKind(objectRef))
	packageDir := "_unknown"
	if trimmed := strings.TrimSpace(objectRef.PackageName); trimmed != "" {
		packageDir = encodeWorkspaceObjectFileName(trimmed)
	}
	fileExtension := dependencyFileExtension(objectRef)
	fileName := url.PathEscape(objectRef.Name) + "." + fileExtension
	return filepath.Join(workspaceRoot, ".abapls", "cache", "packages", packageDir, kindDir, fileName)
}

func ensureDependencyCacheUnit(workspaceRoot string, objectRef AdtObjectRef, filePath string, sourceFiles []string) error {
	relativeFile, err := filepath.Rel(workspaceRoot, filePath)
	if err != nil {
		return fmt.Errorf("relative dependency path %s: %w", filePath, err)
	}
	relativeFile = normalizeRelativePath(relativeFile)
	unitBlock := renderDependencyUnitBlock(objectRef, relativeFile)

	uniqueSources := append([]string(nil), sourceFiles...)
	sort.Strings(uniqueSources)
	uniqueSources = dedupeStrings(uniqueSources)
	for _, sourceFile := range uniqueSources {
		if strings.TrimSpace(sourceFile) == "" {
			continue
		}
		manifestPath := dependencyCacheManifestPath(workspaceRoot, sourceFile)
		if err := os.MkdirAll(filepath.Dir(manifestPath), os.ModePerm); err != nil {
			return fmt.Errorf("create dependency manifest dir %s: %w", filepath.Dir(manifestPath), err)
		}

		existingBytes, err := os.ReadFile(manifestPath)
		if err != nil && !os.IsNotExist(err) {
			return fmt.Errorf("read dependency manifest %s: %w", manifestPath, err)
		}
		existing := string(existingBytes)
		if existing == "" {
			existing = fmt.Sprintf("source_file = %q\n", escapeTomlString(normalizeRelativePath(sourceFile)))
		}
		if strings.Contains(existing, fmt.Sprintf("root_file = %q", escapeTomlString(relativeFile))) &&
			strings.Contains(existing, fmt.Sprintf("name = %q", escapeTomlString(objectRef.Name))) {
			continue
		}

		if !strings.HasSuffix(existing, "\n") {
			existing += "\n"
		}
		existing += unitBlock
		if err := os.WriteFile(manifestPath, []byte(existing), os.ModePerm); err != nil {
			return fmt.Errorf("write dependency manifest %s: %w", manifestPath, err)
		}
	}

	return nil
}

func renderDependencyUnitBlock(objectRef AdtObjectRef, relativeFile string) string {
	var builder strings.Builder
	builder.WriteString("\n[[unit]]\n")
	builder.WriteString(fmt.Sprintf("name = %q\n", escapeTomlString(objectRef.Name)))
	builder.WriteString(fmt.Sprintf("kind = %q\n", escapeTomlString(inferManifestKind(objectRef))))
	builder.WriteString(fmt.Sprintf("root_file = %q\n", escapeTomlString(relativeFile)))
	if trimmed := strings.TrimSpace(objectRef.PackageName); trimmed != "" {
		builder.WriteString(fmt.Sprintf("package_name = %q\n", escapeTomlString(trimmed)))
	}
	return builder.String()
}

func dependencyCacheManifestPath(workspaceRoot string, sourceFile string) string {
	return filepath.Join(
		workspaceRoot,
		".abapls",
		"cache",
		"dependency-manifests",
		url.PathEscape(normalizeRelativePath(sourceFile))+".toml",
	)
}

func writeCachedRemoteObjectMetadata(workspaceRoot string, objectRef AdtObjectRef, fileExtension string, size int) error {
	objectsDir := filepath.Join(workspaceRoot, ".abapls", "cache", "objects")
	if err := os.MkdirAll(objectsDir, os.ModePerm); err != nil {
		return fmt.Errorf("create objects dir %s: %w", objectsDir, err)
	}

	metadataPath := filepath.Join(objectsDir, url.PathEscape(objectRef.Name)+".json")
	payload, err := json.MarshalIndent(CachedRemoteObjectMetadata{
		URI:           objectRef.URI,
		Type:          objectRef.Type,
		Name:          objectRef.Name,
		PackageName:   objectRef.PackageName,
		Description:   objectRef.Description,
		FileExtension: fileExtension,
		Size:          size,
		FetchedAt:     time.Now().UTC().Format(time.RFC3339),
	}, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal object metadata for %s: %w", objectRef.Name, err)
	}
	if err := os.WriteFile(metadataPath, payload, os.ModePerm); err != nil {
		return fmt.Errorf("write object metadata %s: %w", metadataPath, err)
	}
	return nil
}

func hasCachedRemoteDependencyCandidate(workspaceRoot string, candidate RemoteDependencyCandidate) bool {
	metadataPath := filepath.Join(workspaceRoot, ".abapls", "cache", "objects", url.PathEscape(strings.TrimSpace(candidate.Name))+".json")
	if _, err := os.Stat(metadataPath); err == nil {
		return true
	}
	for _, candidatePath := range cachedRemoteDependencyCandidatePaths(workspaceRoot, candidate) {
		if _, err := os.Stat(candidatePath); err == nil {
			return true
		}
	}
	return false
}

func cachedRemoteDependencyCandidatePaths(workspaceRoot string, candidate RemoteDependencyCandidate) []string {
	normalizedName := strings.ToUpper(strings.TrimSpace(candidate.Name))
	if normalizedName == "" {
		return nil
	}

	encodedName := url.PathEscape(normalizedName)
	dependenciesRoot := filepath.Join(workspaceRoot, ".abapls", "cache", "dependencies")
	switch normalizeCandidateKind(candidate.Kind) {
	case "include":
		return []string{filepath.Join(dependenciesRoot, "include", encodedName+".abap")}
	case "message-class":
		return []string{filepath.Join(dependenciesRoot, "message-class", encodedName+".xml")}
	case "function":
		return []string{filepath.Join(dependenciesRoot, "function-group", encodedName+".abap")}
	case "symbol", "static", "type":
		return []string{
			filepath.Join(dependenciesRoot, "global-class", encodedName+".abap"),
			filepath.Join(dependenciesRoot, "global-interface", encodedName+".abap"),
			filepath.Join(dependenciesRoot, "ddic-data-element", encodedName+".xml"),
			filepath.Join(dependenciesRoot, "ddic-structure", encodedName+".xml"),
			filepath.Join(dependenciesRoot, "ddic-table", encodedName+".xml"),
			filepath.Join(dependenciesRoot, "ddic-table-type", encodedName+".xml"),
			filepath.Join(dependenciesRoot, "ddic-view", encodedName+".xml"),
		}
	default:
		return nil
	}
}

func negativeRemoteDependencyMarkerPath(workspaceRoot string, candidate RemoteDependencyCandidate) string {
	normalizedName := strings.ToUpper(strings.TrimSpace(candidate.Name))
	encodedName := url.PathEscape(normalizedName)
	kind := normalizeCandidateKind(candidate.Kind)
	if kind == "" {
		kind = "unknown"
	}
	return filepath.Join(workspaceRoot, ".abapls", "cache", "negative-dependencies", kind, encodedName+".json")
}

func hasNegativeRemoteDependencyCandidate(workspaceRoot string, candidate RemoteDependencyCandidate) bool {
	_, err := os.Stat(negativeRemoteDependencyMarkerPath(workspaceRoot, candidate))
	return err == nil
}

func markNegativeRemoteDependencyCandidate(workspaceRoot string, candidate RemoteDependencyCandidate, reason string) error {
	markerPath := negativeRemoteDependencyMarkerPath(workspaceRoot, candidate)
	if err := os.MkdirAll(filepath.Dir(markerPath), os.ModePerm); err != nil {
		return fmt.Errorf("create negative dependency dir %s: %w", filepath.Dir(markerPath), err)
	}
	payload, err := json.MarshalIndent(map[string]string{
		"name":       strings.TrimSpace(candidate.Name),
		"kind":       normalizeCandidateKind(candidate.Kind),
		"reason":     reason,
		"recordedAt": time.Now().UTC().Format(time.RFC3339),
	}, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal negative dependency marker for %s: %w", candidate.Name, err)
	}
	if err := os.WriteFile(markerPath, payload, os.ModePerm); err != nil {
		return fmt.Errorf("write negative dependency marker %s: %w", markerPath, err)
	}
	return nil
}

func clearNegativeRemoteDependencyCandidate(workspaceRoot string, candidate RemoteDependencyCandidate) error {
	markerPath := negativeRemoteDependencyMarkerPath(workspaceRoot, candidate)
	if err := os.Remove(markerPath); err != nil && !os.IsNotExist(err) {
		return fmt.Errorf("remove negative dependency marker %s: %w", markerPath, err)
	}
	return nil
}

func deleteDependencyCacheContents(workspaceRoot string) error {
	paths := []string{
		filepath.Join(workspaceRoot, ".abapls", "cache", "packages"),
		filepath.Join(workspaceRoot, ".abapls", "cache", "objects"),
		filepath.Join(workspaceRoot, ".abapls", "cache", "dependency-manifests"),
		filepath.Join(workspaceRoot, ".abapls", "cache", "negative-dependencies"),
	}
	for _, path := range paths {
		if err := os.RemoveAll(path); err != nil {
			return fmt.Errorf("remove %s: %w", path, err)
		}
	}
	return nil
}

func inferManifestKind(objectRef AdtObjectRef) string {
	loweredURI := strings.ToLower(objectRef.URI)
	switch {
	case isDdicDependencyObject(objectRef):
		return inferDdicManifestKind(objectRef)
	case isMessageClassDependencyObject(objectRef):
		return "message-class"
	case strings.Contains(loweredURI, "/programs/includes/") || strings.EqualFold(objectRef.Type, "PROG/I"):
		return "include"
	case isFunctionModuleObject(objectRef):
		return "function-module"
	case strings.Contains(loweredURI, "/oo/classes/") || strings.HasPrefix(strings.ToUpper(objectRef.Type), "CLAS/"):
		return "global-class"
	case strings.Contains(loweredURI, "/oo/interfaces/") || strings.HasPrefix(strings.ToUpper(objectRef.Type), "INTF/"):
		return "global-interface"
	case strings.Contains(loweredURI, "/functions/groups/"):
		return "function-group"
	default:
		return "report"
	}
}

func dependencyFileExtension(objectRef AdtObjectRef) string {
	if isXMLDependencyObject(objectRef) {
		return "xml"
	}
	return "abap"
}

func isXMLDependencyObject(objectRef AdtObjectRef) bool {
	return isDdicDependencyObject(objectRef) || isMessageClassDependencyObject(objectRef)
}

func inferDdicManifestKind(objectRef AdtObjectRef) string {
	switch strings.ToUpper(objectRef.Type) {
	case "DTEL/DE":
		return "ddic-data-element"
	case "TABL/DS":
		return "ddic-structure"
	case "TABL/DT":
		return "ddic-table"
	case "TABL/DA", "TTYP/DA":
		return "ddic-table-type"
	case "VIEW/DV":
		return "ddic-view"
	default:
		return "ddic-structure"
	}
}

func encodeWorkspaceObjectFileName(name string) string {
	return url.PathEscape(strings.TrimSpace(strings.ToUpper(name)))
}

func sanitizePathSegment(value string) string {
	normalized := strings.TrimSpace(strings.ToLower(value))
	if normalized == "" {
		return "_unknown"
	}
	return strings.Map(func(r rune) rune {
		switch {
		case r >= 'a' && r <= 'z':
			return r
		case r >= '0' && r <= '9':
			return r
		case r == '-' || r == '_':
			return r
		default:
			return '-'
		}
	}, normalized)
}

func normalizeRelativePath(value string) string {
	return strings.ReplaceAll(strings.TrimSpace(value), "\\", "/")
}

func escapeTomlString(value string) string {
	return strings.ReplaceAll(strings.ReplaceAll(value, "\\", "\\\\"), "\"", "\\\"")
}

func logNegativeCandidateSkip(candidate RemoteDependencyCandidate) {
	log.Printf("skipping negative dependency marker for %s (%s)", candidate.Name, candidate.Kind)
}
