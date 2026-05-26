package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
	"sync"
)

type ObjectFilter struct {
	requested map[string]string
	matched   map[string]struct{}
	mu        sync.Mutex
}

func loadObjectFilter(path string) (*ObjectFilter, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read objects file %s: %w", path, err)
	}

	requested := make(map[string]string)
	for _, rawLine := range strings.Split(string(content), "\n") {
		line := strings.TrimSpace(strings.TrimSuffix(rawLine, "\r"))
		line = strings.TrimPrefix(line, "\uFEFF")
		if line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, ";") {
			continue
		}

		normalized := normalizeObjectFilterName(line)
		if normalized == "" {
			continue
		}
		if _, ok := requested[normalized]; ok {
			continue
		}
		requested[normalized] = line
	}

	if len(requested) == 0 {
		return nil, fmt.Errorf("objects file %s did not contain any object names", path)
	}

	return &ObjectFilter{
		requested: requested,
		matched:   make(map[string]struct{}),
	}, nil
}

func normalizeObjectFilterName(value string) string {
	return strings.ToLower(strings.TrimSpace(value))
}

func (f *ObjectFilter) Match(name string) bool {
	if f == nil {
		return true
	}

	normalized := normalizeObjectFilterName(name)
	if normalized == "" {
		return false
	}

	f.mu.Lock()
	defer f.mu.Unlock()

	if _, ok := f.requested[normalized]; !ok {
		return false
	}
	f.matched[normalized] = struct{}{}
	return true
}

func (f *ObjectFilter) RequestedCount() int {
	if f == nil {
		return 0
	}

	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.requested)
}

func (f *ObjectFilter) Unmatched() []string {
	if f == nil {
		return nil
	}

	f.mu.Lock()
	defer f.mu.Unlock()

	unmatched := make([]string, 0, len(f.requested))
	for normalized, original := range f.requested {
		if _, ok := f.matched[normalized]; ok {
			continue
		}
		unmatched = append(unmatched, original)
	}
	sort.Strings(unmatched)
	return unmatched
}
