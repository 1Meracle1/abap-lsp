package main

import (
	"fmt"
	"net/http"
)

// Newer ADT backends expose system messages as an Atom feed, while older
// systems still accept generic XML for the same CSRF bootstrap request.
const sessionBootstrapAccept = "application/atom+xml;type=feed, application/xml"

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
		"Accept":              sessionBootstrapAccept,
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
