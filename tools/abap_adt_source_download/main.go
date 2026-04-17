package main

import (
	"fmt"
	"log"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

type SapContext struct {
	baseURL           string
	username          string
	password          string
	sapClient         string
	objectFilter      *ObjectFilter
	csrfToken         string
	cookies           []*http.Cookie
	wg                *sync.WaitGroup
	httpClient        *http.Client
	semaphore         chan struct{}
	minRequestSpacing time.Duration
	nextRequestAt     time.Time
	requestMu         sync.Mutex
}

func main() {
	cfg, err := loadConfig(os.Args[1:])
	if err != nil {
		log.Fatal(err)
	}

	ctx := &SapContext{
		baseURL:      cfg.BaseURL,
		username:     cfg.Username,
		password:     cfg.Password,
		sapClient:    cfg.SapClient,
		objectFilter: cfg.ObjectFilter,
	}
	ctx.initHTTPClient(RateLimitConfig{
		RequestsPerMinute: cfg.RequestsPerMinute,
		MaxConcurrent:     cfg.MaxConcurrent,
	})
	if cfg.ObjectFilter == nil {
		if cfg.DependencyInput == nil {
			log.Printf(
				"exporting %d package(s) to %s with %d requests/min and %d concurrent requests",
				len(cfg.Packages),
				cfg.OutputDir,
				cfg.RequestsPerMinute,
				cfg.MaxConcurrent,
			)
		}
	} else {
		log.Printf(
			"exporting %d package(s) to %s with %d requests/min, %d concurrent requests, and %d filtered object(s) from %s",
			len(cfg.Packages),
			cfg.OutputDir,
			cfg.RequestsPerMinute,
			cfg.MaxConcurrent,
			cfg.ObjectFilter.RequestedCount(),
			cfg.ObjectsFile,
		)
	}
	if cfg.DependencyInput != nil {
		log.Printf(
			"resolving %d remote dependency candidate(s) into workspace cache under %s with %d requests/min and %d concurrent requests",
			len(cfg.DependencyInput.Candidates),
			cfg.OutputDir,
			cfg.RequestsPerMinute,
			cfg.MaxConcurrent,
		)
	}

	createDirIfNotExists(cfg.OutputDir)
	if cfg.CleanOutput && cfg.DependencyInput == nil {
		if err := deleteContents(cfg.OutputDir); err != nil {
			log.Fatalf("failed to clean output folder %s: %v", cfg.OutputDir, err)
		}
	}

	ctx.csrfToken, ctx.cookies, err = fetchSystemMessages(ctx)
	if err != nil {
		log.Fatalf("failed to fetch CSRF token: %v", err)
	}

	startExport := time.Now()
	defer func() {
		log.Printf("program execution took %.2f minutes", time.Since(startExport).Minutes())
	}()

	if cfg.DependencyInput != nil {
		if err := exportDependencies(ctx, cfg); err != nil {
			log.Fatal(err)
		}
		return
	}

	ctx.wg = &sync.WaitGroup{}
	for _, packageName := range cfg.Packages {
		ctx.wg.Add(1)
		go visitPackage(ctx, packageName, cfg.OutputDir)
	}
	ctx.wg.Wait()

	if unmatched := ctx.objectFilter.Unmatched(); len(unmatched) > 0 {
		log.Printf("requested object names not found in exported packages (%d): %s", len(unmatched), strings.Join(unmatched, ", "))
	}
}

func createDirIfNotExists(dirPath string) (alreadyExists bool) {
	err := os.MkdirAll(dirPath, os.ModePerm)
	if err != nil {
		alreadyExists = os.IsExist(err)
		if !alreadyExists {
			log.Panicf("failed to create directory %s, err: %v", dirPath, err)
		}
	}
	return
}

func dirExists(path string) bool {
	info, err := os.Stat(path)
	if err != nil {
		return false
	}
	return info.IsDir()
}

func fileExists(path string) bool {
	info, err := os.Stat(path)
	if err != nil {
		return false
	}
	return !info.IsDir()
}

func encodeObjectName(objectName string) string {
	return url.PathEscape(objectName)
}

func (ctx *SapContext) shouldFetchObject(name string) bool {
	if ctx == nil {
		return false
	}
	return ctx.objectFilter.Match(name)
}

func deleteContents(dir string) error {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return fmt.Errorf("failed to read directory %s: %w", dir, err)
	}

	for _, entry := range entries {
		entryPath := filepath.Join(dir, entry.Name())
		if entry.IsDir() {
			if err := os.RemoveAll(entryPath); err != nil {
				return fmt.Errorf("failed to remove directory %s: %w", entryPath, err)
			}
		} else {
			if err := os.Remove(entryPath); err != nil {
				return fmt.Errorf("failed to remove file %s: %w", entryPath, err)
			}
		}
	}
	return nil
}
