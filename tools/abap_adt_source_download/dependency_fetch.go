package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

type AdtObjectRef struct {
	URI         string
	Type        string
	Name        string
	PackageName string
	Description string
}

type RemoteDependencyCandidate struct {
	Name string `json:"name"`
	Kind string `json:"kind"`
}

type AdtDependencyFetchResult struct {
	Body               string
	FileExtension      string
	ManifestKind       string
	SharedDependencies []AdtDependencyArtifact
}

type AdtDependencyArtifact struct {
	ObjectRef     AdtObjectRef
	Body          string
	FileExtension string
	ManifestKind  string
}

func resolveDependencyObjects(ctx *SapContext, candidate RemoteDependencyCandidate) ([]AdtObjectRef, bool, error) {
	if normalizeCandidateKind(candidate.Kind) == "message-class" {
		objectRef := buildMessageClassObjectRef(candidate.Name)
		return []AdtObjectRef{objectRef}, false, nil
	}

	objects, err := searchRepositoryObjects(ctx, candidate.Name, 25)
	if err != nil {
		return nil, false, err
	}

	selected := selectDependencyObjects(candidate.Name, objects, candidate.Kind)
	if len(selected) == 0 {
		return nil, false, nil
	}
	return selected, false, nil
}

func fetchDependencyObject(ctx *SapContext, objectRef AdtObjectRef) (AdtDependencyFetchResult, error) {
	if isMessageClassDependencyObject(objectRef) {
		body, err := fetchMessageClass(ctx, objectRef.Name)
		if err != nil {
			return AdtDependencyFetchResult{}, err
		}
		return AdtDependencyFetchResult{
			Body:          body,
			FileExtension: "xml",
			ManifestKind:  "message-class",
		}, nil
	}

	if isFetchableDdicDependencyObject(objectRef) {
		body, err := fetchDdicDependency(ctx, objectRef)
		if err != nil {
			return AdtDependencyFetchResult{}, err
		}
		return AdtDependencyFetchResult{
			Body:          body,
			FileExtension: "xml",
			ManifestKind:  inferDdicManifestKind(objectRef),
		}, nil
	}

	if isFunctionModuleObject(objectRef) {
		return fetchFunctionModuleDependencySource(ctx, objectRef)
	}

	body, err := fetchObjectSource(ctx, objectRef.URI)
	if err != nil {
		return AdtDependencyFetchResult{}, err
	}
	return AdtDependencyFetchResult{
		Body:          body,
		FileExtension: "abap",
		ManifestKind:  inferManifestKind(objectRef),
	}, nil
}

func searchRepositoryObjects(ctx *SapContext, query string, maxResults int) ([]AdtObjectRef, error) {
	rawURL := ctx.adtURL("/repository/informationsystem/search")
	parsed, err := url.Parse(rawURL)
	if err != nil {
		return nil, fmt.Errorf("parse search URL %q: %w", rawURL, err)
	}
	values := parsed.Query()
	values.Set("operation", "quickSearch")
	values.Set("query", query)
	values.Set("maxResults", fmt.Sprintf("%d", maxResults))
	parsed.RawQuery = values.Encode()

	headers := map[string]string{
		"Accept":              "application/xml",
		"X-sap-adt-profiling": "server-time",
		"Cache-Control":       "no-cache",
		"x-csrf-token":        ctx.csrfToken,
	}
	body, err := ctx.fetchWithRateLimit(parsed.String(), headers)
	if err != nil {
		return nil, err
	}
	return parseObjectReferences(body), nil
}

func fetchObjectSource(ctx *SapContext, objectURI string) (string, error) {
	if strings.TrimSpace(objectURI) == "" {
		return "", fmt.Errorf("object URI is empty")
	}
	sourceURI := objectURI
	if !strings.HasSuffix(strings.ToLower(sourceURI), "/source/main") {
		sourceURI += "/source/main"
	}
	headers := map[string]string{
		"Accept":              "text/plain",
		"X-sap-adt-profiling": "server-time",
		"Cache-Control":       "no-cache",
		"x-csrf-token":        ctx.csrfToken,
	}
	return ctx.fetchWithRateLimit(ctx.adtURL(sourceURI), headers)
}

func fetchDdicDependency(ctx *SapContext, objectRef AdtObjectRef) (string, error) {
	encodedName := encodeObjectName(objectRef.Name)
	switch strings.ToUpper(objectRef.Type) {
	case "DTEL/DE":
		return fetchDictDataElement(ctx, encodedName)
	default:
		return fetchDictElementInfo(ctx, encodedName)
	}
}

func fetchMessageClass(ctx *SapContext, name string) (string, error) {
	headers := map[string]string{
		"Accept":              "application/vnd.sap.adt.elementinfo+xml",
		"X-sap-adt-profiling": "server-time",
		"Cache-Control":       "no-cache",
		"x-csrf-token":        ctx.csrfToken,
	}
	return ctx.fetchWithRateLimit(ctx.adtURL("/messageclass/"+encodeObjectName(name)), headers)
}

func fetchFunctionModuleDependencySource(ctx *SapContext, objectRef AdtObjectRef) (AdtDependencyFetchResult, error) {
	functionModuleSource, err := fetchObjectSource(ctx, objectRef.URI)
	if err != nil {
		return AdtDependencyFetchResult{}, err
	}

	functionGroupURI := inferFunctionGroupURI(objectRef)
	if functionGroupURI == "" {
		return AdtDependencyFetchResult{
			Body:          functionModuleSource,
			FileExtension: "abap",
			ManifestKind:  "function-module",
		}, nil
	}

	functionGroupSource, err := fetchObjectSource(ctx, functionGroupURI)
	if err != nil {
		return AdtDependencyFetchResult{
			Body:          functionModuleSource,
			FileExtension: "abap",
			ManifestKind:  "function-module",
		}, nil
	}

	var sharedDependencies []AdtDependencyArtifact
	for _, includeName := range extractActiveTopLevelIncludeNames(functionGroupSource) {
		if isFunctionGroupDispatcherInclude(includeName) {
			continue
		}
		includeSource, err := fetchObjectSource(ctx, "/programs/includes/"+encodeObjectName(includeName))
		if err != nil {
			continue
		}
		sharedDependencies = append(sharedDependencies, AdtDependencyArtifact{
			ObjectRef: AdtObjectRef{
				URI:         "/sap/bc/adt/programs/includes/" + encodeObjectName(includeName),
				Type:        "PROG/I",
				Name:        strings.ToUpper(strings.TrimSpace(includeName)),
				PackageName: objectRef.PackageName,
				Description: "Include",
			},
			Body:          includeSource,
			FileExtension: "abap",
			ManifestKind:  "include",
		})
	}
	sort.Slice(sharedDependencies, func(i, j int) bool {
		return sharedDependencies[i].ObjectRef.Name < sharedDependencies[j].ObjectRef.Name
	})

	return AdtDependencyFetchResult{
		Body:               buildFunctionModuleDependencySource(functionGroupSource, functionModuleSource),
		FileExtension:      "abap",
		ManifestKind:       "function-module",
		SharedDependencies: sharedDependencies,
	}, nil
}

func isSupportedDependencyObject(objectRef AdtObjectRef, kindHint string) bool {
	loweredType := strings.ToUpper(objectRef.Type)
	loweredURI := strings.ToLower(objectRef.URI)

	switch normalizeCandidateKind(kindHint) {
	case "message-class":
		return isMessageClassDependencyObject(objectRef)
	case "include":
		return strings.Contains(loweredURI, "/programs/includes/") || loweredType == "PROG/I"
	case "report":
		return strings.Contains(loweredURI, "/programs/programs/") || loweredType == "PROG/P"
	case "function":
		return strings.Contains(loweredURI, "/functions/groups/") || loweredType == "FUGR/F" || loweredType == "FUGR/FF"
	case "static":
		return strings.Contains(loweredURI, "/oo/classes/") ||
			strings.Contains(loweredURI, "/oo/interfaces/") ||
			strings.HasPrefix(loweredType, "CLAS/") ||
			strings.HasPrefix(loweredType, "INTF/")
	case "type":
		return isFetchableDdicDependencyObject(objectRef) ||
			strings.Contains(loweredURI, "/oo/classes/") ||
			strings.Contains(loweredURI, "/oo/interfaces/") ||
			strings.HasPrefix(loweredType, "CLAS/") ||
			strings.HasPrefix(loweredType, "INTF/")
	}

	return strings.Contains(loweredURI, "/programs/includes/") ||
		strings.Contains(loweredURI, "/programs/programs/") ||
		strings.Contains(loweredURI, "/oo/classes/") ||
		strings.Contains(loweredURI, "/oo/interfaces/") ||
		strings.Contains(loweredURI, "/functions/groups/") ||
		isMessageClassDependencyObject(objectRef) ||
		isFetchableDdicDependencyObject(objectRef) ||
		loweredType == "PROG/I" ||
		loweredType == "PROG/P" ||
		strings.HasPrefix(loweredType, "CLAS/") ||
		strings.HasPrefix(loweredType, "INTF/")
}

func isDdicDomainObject(objectRef AdtObjectRef) bool {
	return strings.HasPrefix(strings.ToUpper(objectRef.Type), "DOMA/")
}

func isFetchableDdicDependencyObject(objectRef AdtObjectRef) bool {
	return isDdicDependencyObject(objectRef) && !isDdicDomainObject(objectRef)
}

func selectDependencyObjects(query string, objects []AdtObjectRef, kindHint string) []AdtObjectRef {
	normalizedQuery := normalizeRemoteDependencyName(query)
	if normalizedQuery == "" {
		return nil
	}

	var supportedExact []AdtObjectRef
	for _, objectRef := range objects {
		if normalizeRemoteDependencyName(objectRef.Name) == normalizedQuery &&
			isSupportedDependencyObject(objectRef, "") {
			supportedExact = append(supportedExact, objectRef)
		}
	}
	if len(supportedExact) > 0 {
		return dropShadowedDdicDomainObjects(dedupeAndSortDependencyObjects(supportedExact))
	}

	var supportedByHint []AdtObjectRef
	for _, objectRef := range objects {
		if isSupportedDependencyObject(objectRef, kindHint) {
			supportedByHint = append(supportedByHint, objectRef)
		}
	}
	if len(supportedByHint) == 0 {
		for _, objectRef := range objects {
			if isSupportedDependencyObject(objectRef, "") {
				supportedByHint = append(supportedByHint, objectRef)
			}
		}
	}
	if len(supportedByHint) == 0 {
		return nil
	}

	if preferred := pickBestDependencyObject(query, supportedByHint, kindHint); preferred != nil {
		return []AdtObjectRef{*preferred}
	}
	return []AdtObjectRef{supportedByHint[0]}
}

func dedupeAndSortDependencyObjects(objects []AdtObjectRef) []AdtObjectRef {
	seen := map[string]struct{}{}
	out := make([]AdtObjectRef, 0, len(objects))
	for _, objectRef := range objects {
		key := strings.ToUpper(strings.TrimSpace(objectRef.Type)) + "::" + strings.ToLower(strings.TrimSpace(objectRef.URI))
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		out = append(out, objectRef)
	}
	sort.Slice(out, func(i, j int) bool {
		left := strings.ToUpper(strings.TrimSpace(out[i].Type))
		right := strings.ToUpper(strings.TrimSpace(out[j].Type))
		if left != right {
			return left < right
		}
		return strings.ToLower(out[i].URI) < strings.ToLower(out[j].URI)
	})
	return out
}

func dropShadowedDdicDomainObjects(objects []AdtObjectRef) []AdtObjectRef {
	hasConcreteDdicObject := false
	for _, objectRef := range objects {
		if isDdicDependencyObject(objectRef) && !isDdicDomainObject(objectRef) {
			hasConcreteDdicObject = true
			break
		}
	}
	if !hasConcreteDdicObject {
		return objects
	}

	out := make([]AdtObjectRef, 0, len(objects))
	for _, objectRef := range objects {
		if !isDdicDomainObject(objectRef) {
			out = append(out, objectRef)
		}
	}
	return out
}

func pickBestDependencyObject(query string, objects []AdtObjectRef, kindHint string) *AdtObjectRef {
	normalizedQuery := normalizeRemoteDependencyName(query)
	if normalizedQuery == "" {
		return nil
	}

	var supported []AdtObjectRef
	for _, objectRef := range objects {
		if isSupportedDependencyObject(objectRef, kindHint) {
			supported = append(supported, objectRef)
		}
	}
	if len(supported) == 0 {
		return nil
	}

	var exactMatches []AdtObjectRef
	for _, objectRef := range supported {
		if normalizeRemoteDependencyName(objectRef.Name) == normalizedQuery {
			exactMatches = append(exactMatches, objectRef)
		}
	}
	if preferred := pickPreferredDependencyObject(exactMatches, kindHint); preferred != nil {
		return preferred
	}
	if len(exactMatches) > 0 {
		objectRef := exactMatches[0]
		return &objectRef
	}

	if preferred := pickPreferredDependencyObject(supported, kindHint); preferred != nil {
		return preferred
	}
	objectRef := supported[0]
	return &objectRef
}

func pickPreferredDependencyObject(objects []AdtObjectRef, kindHint string) *AdtObjectRef {
	if len(objects) == 0 {
		return nil
	}

	switch normalizeCandidateKind(kindHint) {
	case "report":
		for _, objectRef := range objects {
			if strings.EqualFold(objectRef.Type, "PROG/P") {
				ref := objectRef
				return &ref
			}
		}
	case "function":
		for _, objectRef := range objects {
			if strings.EqualFold(objectRef.Type, "FUGR/FF") {
				ref := objectRef
				return &ref
			}
		}
		for _, objectRef := range objects {
			if strings.EqualFold(objectRef.Type, "FUGR/F") {
				ref := objectRef
				return &ref
			}
		}
	case "static":
		for _, objectRef := range objects {
			if strings.HasPrefix(strings.ToUpper(objectRef.Type), "CLAS/") {
				ref := objectRef
				return &ref
			}
		}
		for _, objectRef := range objects {
			if strings.HasPrefix(strings.ToUpper(objectRef.Type), "INTF/") {
				ref := objectRef
				return &ref
			}
		}
	case "type":
		for _, objectRef := range objects {
			if isDdicDependencyObject(objectRef) && !isDdicDomainObject(objectRef) {
				ref := objectRef
				return &ref
			}
		}
		for _, objectRef := range objects {
			if isDdicDomainObject(objectRef) {
				ref := objectRef
				return &ref
			}
		}
		for _, objectRef := range objects {
			if strings.HasPrefix(strings.ToUpper(objectRef.Type), "CLAS/") {
				ref := objectRef
				return &ref
			}
		}
		for _, objectRef := range objects {
			if strings.HasPrefix(strings.ToUpper(objectRef.Type), "INTF/") {
				ref := objectRef
				return &ref
			}
		}
	}

	return nil
}

func isDdicDependencyObject(objectRef AdtObjectRef) bool {
	switch strings.ToUpper(objectRef.Type) {
	case "DTEL/DE", "TABL/DS", "TABL/DT", "TABL/DA", "TTYP/DA", "VIEW/DV":
		return true
	default:
		return isDdicDomainObject(objectRef)
	}
}

func isMessageClassDependencyObject(objectRef AdtObjectRef) bool {
	return strings.EqualFold(objectRef.Type, "MSAG/N") || strings.Contains(strings.ToLower(objectRef.URI), "/sap/bc/adt/messageclass/")
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

func inferDdicManifestKind(objectRef AdtObjectRef) string {
	upperType := strings.ToUpper(objectRef.Type)
	switch {
	case upperType == "DTEL/DE":
		return "ddic-data-element"
	case isDdicDomainObject(objectRef):
		return "ddic-domain"
	case upperType == "TABL/DS":
		return "ddic-structure"
	case upperType == "TABL/DT":
		return "ddic-table"
	case upperType == "TABL/DA" || upperType == "TTYP/DA":
		return "ddic-table-type"
	case upperType == "VIEW/DV":
		return "ddic-view"
	default:
		return "ddic-structure"
	}
}

func buildMessageClassObjectRef(name string) AdtObjectRef {
	normalizedName := strings.ToUpper(strings.TrimSpace(name))
	return AdtObjectRef{
		URI:         "/sap/bc/adt/messageclass/" + encodeObjectName(normalizedName),
		Type:        "MSAG/N",
		Name:        normalizedName,
		PackageName: "",
		Description: "Message class",
	}
}

func isFunctionModuleObject(objectRef AdtObjectRef) bool {
	return strings.EqualFold(objectRef.Type, "FUGR/FF") ||
		(strings.Contains(strings.ToLower(objectRef.URI), "/functions/groups/") &&
			strings.Contains(strings.ToLower(objectRef.URI), "/fmodules/"))
}

func inferFunctionGroupURI(objectRef AdtObjectRef) string {
	matcher := regexp.MustCompile(`(?i)^(.*?/functions/groups/[^/]+)(?:/fmodules/[^/]+)?$`)
	match := matcher.FindStringSubmatch(objectRef.URI)
	if len(match) < 2 {
		return ""
	}
	return match[1]
}

func extractActiveTopLevelIncludeNames(source string) []string {
	var includeNames []string
	seen := make(map[string]struct{})
	for _, rawLine := range strings.Split(normalizeABAPSource(source), "\n") {
		includeName := activeIncludeNameFromLine(rawLine)
		if includeName == "" {
			continue
		}
		if _, ok := seen[includeName]; ok {
			continue
		}
		seen[includeName] = struct{}{}
		includeNames = append(includeNames, includeName)
	}
	return includeNames
}

func buildFunctionModuleDependencySource(functionGroupSource string, functionModuleSource string) string {
	renderedGroup := make([]string, 0)
	for _, rawLine := range strings.Split(normalizeABAPSource(functionGroupSource), "\n") {
		includeName := activeIncludeNameFromLine(rawLine)
		if includeName == "" {
			renderedGroup = append(renderedGroup, rawLine)
			continue
		}
		if isFunctionGroupDispatcherInclude(includeName) {
			renderedGroup = append(renderedGroup, "* INCLUDE "+includeName+". Omitted in dependency cache; function module stays in its own unit.")
			continue
		}
		renderedGroup = append(renderedGroup, rawLine)
	}

	return trimTrailingWhitespace(strings.Join(renderedGroup, "\n")) + "\n\n" + trimTrailingWhitespace(normalizeABAPSource(functionModuleSource)) + "\n"
}

func activeIncludeNameFromLine(line string) string {
	trimmed := strings.TrimSpace(line)
	if strings.HasPrefix(trimmed, "*") {
		return ""
	}

	withoutComment := line
	if idx := strings.IndexByte(withoutComment, '"'); idx >= 0 {
		withoutComment = withoutComment[:idx]
	}
	matcher := regexp.MustCompile(`(?i)^\s*include\s+([^\s.]+)\s*\.\s*$`)
	match := matcher.FindStringSubmatch(withoutComment)
	if len(match) < 2 {
		return ""
	}
	return strings.ToUpper(strings.TrimSpace(match[1]))
}

func isFunctionGroupDispatcherInclude(includeName string) bool {
	return strings.HasSuffix(strings.ToUpper(strings.TrimSpace(includeName)), "UXX")
}

func normalizeABAPSource(source string) string {
	return strings.ReplaceAll(source, "\r\n", "\n")
}

func trimTrailingWhitespace(source string) string {
	return strings.TrimRight(source, " \t\r\n")
}

func parseObjectReferences(xml string) []AdtObjectRef {
	matcher := regexp.MustCompile(`(?i)<adtcore:objectReference\b([^>]*)/>`)
	matches := matcher.FindAllStringSubmatch(xml, -1)
	results := make([]AdtObjectRef, 0, len(matches))
	for _, match := range matches {
		attributes := ""
		if len(match) > 1 {
			attributes = match[1]
		}
		entry := AdtObjectRef{
			URI:         decodeXMLEntity(readAttribute(attributes, "adtcore:uri")),
			Type:        decodeXMLEntity(readAttribute(attributes, "adtcore:type")),
			Name:        decodeXMLEntity(readAttribute(attributes, "adtcore:name")),
			PackageName: decodeXMLEntity(readAttribute(attributes, "adtcore:packageName")),
			Description: decodeXMLEntity(readAttribute(attributes, "adtcore:description")),
		}
		if strings.TrimSpace(entry.URI) == "" || strings.TrimSpace(entry.Name) == "" {
			continue
		}
		results = append(results, entry)
	}
	return results
}

func readAttribute(attributes string, name string) string {
	escapedName := regexp.QuoteMeta(name)
	matcher := regexp.MustCompile(`(?i)` + escapedName + `="([^"]*)"`)
	match := matcher.FindStringSubmatch(attributes)
	if len(match) < 2 {
		return ""
	}
	return match[1]
}

func decodeXMLEntity(value string) string {
	replacer := strings.NewReplacer(
		"&quot;", `"`,
		"&apos;", "'",
		"&lt;", "<",
		"&gt;", ">",
		"&amp;", "&",
	)
	return replacer.Replace(value)
}

func workspaceRelativePathFromURI(workspaceRoot string, sourceURI string) (string, error) {
	path, err := fileURIToPath(sourceURI)
	if err != nil {
		return "", err
	}
	relative, err := filepath.Rel(workspaceRoot, path)
	if err != nil {
		return "", fmt.Errorf("relative source path %s: %w", path, err)
	}
	if strings.HasPrefix(relative, "..") {
		return "", fmt.Errorf("source URI %s is outside workspace root %s", sourceURI, workspaceRoot)
	}
	return normalizeRelativePath(relative), nil
}

func fileURIToPath(uri string) (string, error) {
	parsed, err := url.Parse(uri)
	if err != nil {
		return "", fmt.Errorf("parse file URI %q: %w", uri, err)
	}
	if !strings.EqualFold(parsed.Scheme, "file") {
		return "", fmt.Errorf("unsupported source URI scheme for %q", uri)
	}
	decodedPath, err := url.PathUnescape(parsed.Path)
	if err != nil {
		return "", fmt.Errorf("decode file URI path %q: %w", uri, err)
	}
	if decodedPath == "" {
		return "", fmt.Errorf("file URI %q did not contain a path", uri)
	}
	if len(decodedPath) >= 3 && decodedPath[0] == '/' && decodedPath[2] == ':' {
		decodedPath = decodedPath[1:]
	}
	return filepath.Clean(strings.ReplaceAll(decodedPath, "/", string(filepath.Separator))), nil
}

func normalizeRelativePath(value string) string {
	return strings.ReplaceAll(strings.TrimSpace(value), "\\", "/")
}

func fetchRaw(ctx *SapContext, rawURL string, method string, accept string, contentType string, body []byte) (string, error) {
	req, err := http.NewRequest(method, rawURL, bytes.NewReader(body))
	if err != nil {
		return "", err
	}
	headers := map[string]string{
		"Accept":              accept,
		"X-sap-adt-profiling": "server-time",
		"Cache-Control":       "no-cache",
		"x-csrf-token":        ctx.csrfToken,
	}
	if strings.TrimSpace(contentType) != "" {
		headers["Content-Type"] = contentType
	}
	resp, err := ctx.doRequestWithHeaders(req, headers)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	if resp.StatusCode > 299 {
		return "", fmt.Errorf("HTTP response code: %d, response body: %q", resp.StatusCode, data)
	}
	return string(data), nil
}

func logDependencyResolution(candidate RemoteDependencyCandidate, objectRef *AdtObjectRef) {
	if objectRef == nil {
		log.Printf("no supported ADT dependency match for %s (%s)", candidate.Name, candidate.Kind)
		return
	}
	log.Printf("resolved %s (%s) to %s [%s]", candidate.Name, candidate.Kind, objectRef.Name, objectRef.Type)
}

func normalizeRemoteDependencyName(name string) string {
	return strings.ToLower(strings.TrimSpace(name))
}

func normalizeCandidateKind(kind string) string {
	return strings.ToLower(strings.TrimSpace(kind))
}
