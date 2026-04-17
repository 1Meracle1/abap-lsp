package main

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"
)

type RateLimitConfig struct {
	RequestsPerMinute int
	MaxConcurrent     int
}

func DefaultRateLimitConfig() RateLimitConfig {
	return RateLimitConfig{
		RequestsPerMinute: defaultRequestsPerMinute,
		MaxConcurrent:     defaultMaxConcurrent,
	}
}

func (ctx *SapContext) initHTTPClient(config RateLimitConfig) {
	if config.RequestsPerMinute <= 0 {
		config.RequestsPerMinute = defaultRequestsPerMinute
	}
	if config.MaxConcurrent <= 0 {
		config.MaxConcurrent = defaultMaxConcurrent
	}

	ctx.httpClient = &http.Client{
		Timeout: 60 * time.Second,
		Transport: &http.Transport{
			MaxIdleConns:        100,
			MaxIdleConnsPerHost: 100,
			IdleConnTimeout:     90 * time.Second,
		},
	}
	ctx.minRequestSpacing = time.Minute / time.Duration(config.RequestsPerMinute)
	ctx.semaphore = make(chan struct{}, config.MaxConcurrent)
}

func (ctx *SapContext) acquireRequestSlot() {
	ctx.semaphore <- struct{}{}

	ctx.requestMu.Lock()
	now := time.Now()
	wait := time.Duration(0)
	if now.Before(ctx.nextRequestAt) {
		wait = ctx.nextRequestAt.Sub(now)
		now = ctx.nextRequestAt
	}
	ctx.nextRequestAt = now.Add(ctx.minRequestSpacing)
	ctx.requestMu.Unlock()

	if wait > 0 {
		time.Sleep(wait)
	}
}

func (ctx *SapContext) releaseRequestSlot() {
	<-ctx.semaphore
}

func (ctx *SapContext) doRequest(req *http.Request) (*http.Response, error) {
	ctx.acquireRequestSlot()
	defer ctx.releaseRequestSlot()

	for _, cookie := range ctx.cookies {
		req.AddCookie(cookie)
	}
	if ctx.sapClient != "" {
		query := req.URL.Query()
		if query.Get("sap-client") == "" {
			query.Set("sap-client", ctx.sapClient)
			req.URL.RawQuery = query.Encode()
		}
	}
	req.SetBasicAuth(ctx.username, ctx.password)
	return ctx.httpClient.Do(req)
}

func (ctx *SapContext) doRequestWithHeaders(req *http.Request, headers map[string]string) (*http.Response, error) {
	for key, value := range headers {
		req.Header.Set(key, value)
	}
	return ctx.doRequest(req)
}

func (ctx *SapContext) fetchWithRateLimit(rawURL string, headers map[string]string) (string, error) {
	req, err := http.NewRequest("GET", rawURL, nil)
	if err != nil {
		return "", err
	}

	resp, err := ctx.doRequestWithHeaders(req, headers)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	if resp.StatusCode > 299 {
		return "", fmt.Errorf("HTTP response code: %d, response body: %q", resp.StatusCode, body)
	}

	return string(body), nil
}

func (ctx *SapContext) adtURL(relativePath string) string {
	base := strings.TrimRight(ctx.baseURL, "/")
	if strings.HasPrefix(relativePath, "http://") || strings.HasPrefix(relativePath, "https://") {
		return relativePath
	}
	if strings.HasPrefix(relativePath, "/sap/bc/adt") {
		if strings.HasSuffix(strings.ToLower(base), "/sap/bc/adt") {
			return strings.TrimSuffix(base, "/sap/bc/adt") + relativePath
		}
		return base + relativePath
	}
	if strings.HasPrefix(relativePath, "/") {
		return base + relativePath
	}
	return base + "/" + relativePath
}

func addQueryParam(rawURL, key, value string) (string, error) {
	if strings.TrimSpace(value) == "" {
		return rawURL, nil
	}
	parsed, err := url.Parse(rawURL)
	if err != nil {
		return "", fmt.Errorf("parse URL %q: %w", rawURL, err)
	}
	query := parsed.Query()
	if query.Get(key) == "" {
		query.Set(key, value)
		parsed.RawQuery = query.Encode()
	}
	return parsed.String(), nil
}
