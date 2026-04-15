package main

import (
	"fmt"
	"log"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"sync"
	"time"
)

type SapContext struct {
	baseURL           string
	username          string
	password          string
	sapClient         string
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
		baseURL:   cfg.BaseURL,
		username:  cfg.Username,
		password:  cfg.Password,
		sapClient: cfg.SapClient,
	}
	ctx.initHTTPClient(RateLimitConfig{
		RequestsPerMinute: cfg.RequestsPerMinute,
		MaxConcurrent:     cfg.MaxConcurrent,
	})
	log.Printf(
		"exporting %d package(s) to %s with %d requests/min and %d concurrent requests",
		len(cfg.Packages),
		cfg.OutputDir,
		cfg.RequestsPerMinute,
		cfg.MaxConcurrent,
	)

	createDirIfNotExists(cfg.OutputDir)
	if cfg.CleanOutput {
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

	ctx.wg = &sync.WaitGroup{}
	for _, packageName := range cfg.Packages {
		ctx.wg.Add(1)
		go visitPackage(ctx, packageName, cfg.OutputDir)
	}
	ctx.wg.Wait()
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
