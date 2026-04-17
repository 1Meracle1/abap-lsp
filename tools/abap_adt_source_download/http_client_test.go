package main

import "testing"

func TestAdtURLKeepsSingleAdtPrefixForSearchResultURIs(t *testing.T) {
	t.Parallel()

	ctx := &SapContext{
		baseURL: "https://sap.example.com/sap/bc/adt",
	}

	got := ctx.adtURL("/sap/bc/adt/functions/groups/zvle_class")
	want := "https://sap.example.com/sap/bc/adt/functions/groups/zvle_class"
	if got != want {
		t.Fatalf("adtURL() = %q, want %q", got, want)
	}
}

func TestAdtURLStillJoinsRelativeAdtPaths(t *testing.T) {
	t.Parallel()

	ctx := &SapContext{
		baseURL: "https://sap.example.com/sap/bc/adt",
	}

	got := ctx.adtURL("/functions/groups/zvle_class")
	want := "https://sap.example.com/sap/bc/adt/functions/groups/zvle_class"
	if got != want {
		t.Fatalf("adtURL() = %q, want %q", got, want)
	}
}
