package main

import "fmt"

func fetchReportSource(ctx *SapContext, encodedName string) (src string, err error) {
	return fetchSource(ctx, "/programs/programs/", encodedName)
}

func fetchIncludeSource(ctx *SapContext, encodedName string) (src string, err error) {
	return fetchSource(ctx, "/programs/includes/", encodedName)
}

func fetchClassSource(ctx *SapContext, encodedName string) (src string, err error) {
	return fetchSource(ctx, "/oo/classes/", encodedName)
}

func fetchInterfaceSource(ctx *SapContext, encodedName string) (src string, err error) {
	return fetchSource(ctx, "/oo/interfaces/", encodedName)
}

func fetchFunctionGroupSource(ctx *SapContext, encodedName string) (src string, err error) {
	return fetchSource(ctx, "/functions/groups/", encodedName)
}

func fetchFunctionModuleSource(ctx *SapContext, encodedFunctionGroupName string, encodedFunctionModuleName string) (src string, err error) {
	return fetchSource(ctx, "/functions/groups/", encodedFunctionGroupName+"/fmodules/"+encodedFunctionModuleName)
}

func fetchSource(ctx *SapContext, uriForType string, encodedName string) (src string, err error) {
	if ctx == nil {
		return src, fmt.Errorf("sap context is nil")
	}

	headers := map[string]string{
		"Accept":              "text/plain",
		"X-sap-adt-profiling": "server-time",
		"Cache-Control":       "no-cache",
		"x-csrf-token":        ctx.csrfToken,
	}

	rawURL := ctx.adtURL(uriForType + encodedName + "/source/main")
	return ctx.fetchWithRateLimit(rawURL, headers)
}
