package main

import (
	"fmt"
	"net/http"
)

func fetchSystemMessages(ctx *SapContext) (csrfToken string, cookies []*http.Cookie, err error) {
	client := ctx.httpClient
	if client == nil {
		client = &http.Client{}
	}

	systemMessagesURL, err := addQueryParam(ctx.adtURL("/runtime/systemmessages"), "sap-client", ctx.sapClient)
	if err != nil {
		return "", nil, err
	}
	req, err := http.NewRequest("GET", systemMessagesURL, nil)
	if err != nil {
		return "", nil, err
	}

	headers := map[string]string{
		"Cache-Control":       "no-cache",
		"Accept":              "application/xml",
		"X-sap-adt-profiling": "server-time",
		"x-csrf-token":        "Fetch",
	}
	for key, value := range headers {
		req.Header.Set(key, value)
	}
	req.SetBasicAuth(ctx.username, ctx.password)

	resp, err := client.Do(req)
	if err != nil {
		return "", nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode > 300 {
		return "", nil, fmt.Errorf("HTTP response %d", resp.StatusCode)
	}
	return resp.Header.Get("x-csrf-token"), resp.Cookies(), nil
}
