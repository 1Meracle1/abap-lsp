package main

const dataElementAccept = "application/vnd.sap.adt.dataelements.v1+xml, application/vnd.sap.adt.dataelements.v2+xml"

func fetchDictDataElement(ctx *SapContext, encodedName string) (src string, err error) {
	return fetchDictionary(ctx, "/ddic/dataelements/", encodedName, dataElementAccept)
}

func fetchDictionary(ctx *SapContext, pathType string, encodedName string, accept string) (src string, err error) {
	headers := map[string]string{
		"Accept":              accept,
		"X-sap-adt-profiling": "server-time",
		"Cache-Control":       "no-cache",
		"x-csrf-token":        ctx.csrfToken,
	}

	rawURL := ctx.adtURL(pathType + encodedName)
	return ctx.fetchWithRateLimit(rawURL, headers)
}

func fetchDictElementInfo(ctx *SapContext, encodedName string) (src string, err error) {
	headers := map[string]string{
		"Accept":              "application/vnd.sap.adt.elementinfo+xml",
		"X-sap-adt-profiling": "server-time",
		"Cache-Control":       "no-cache",
		"x-csrf-token":        ctx.csrfToken,
	}

	rawURL := ctx.adtURL("/ddic/elementinfo?path=" + encodedName)
	return ctx.fetchWithRateLimit(rawURL, headers)
}
