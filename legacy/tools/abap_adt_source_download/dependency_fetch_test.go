package main

import "testing"

func TestSelectDependencyObjectsReturnsAllSupportedExactMatches(t *testing.T) {
	objects := []AdtObjectRef{
		{
			URI:         "/sap/bc/adt/ddic/dbtables/ekko",
			Type:        "TABL/DT",
			Name:        "EKKO",
			PackageName: "SABAP",
			Description: "Purchasing document header",
		},
		{
			URI:         "/sap/bc/adt/functions/groups/mm06e0/fmodules/ekko",
			Type:        "FUGR/FF",
			Name:        "EKKO",
			PackageName: "MM06E0",
			Description: "Function module",
		},
	}

	selected := selectDependencyObjects("ekko", objects, "type")
	if len(selected) != 2 {
		t.Fatalf("expected both exact matches, got %#v", selected)
	}
}

func TestSelectDependencyObjectsPrefersReportsForReportCandidates(t *testing.T) {
	objects := []AdtObjectRef{
		{
			URI:         "/sap/bc/adt/programs/programs/rsnast00",
			Type:        "PROG/P",
			Name:        "RSNAST00",
			PackageName: "VN",
			Description: "Report",
		},
		{
			URI:         "/sap/bc/adt/programs/includes/rsnast00",
			Type:        "PROG/I",
			Name:        "RSNAST00",
			PackageName: "VN",
			Description: "Include",
		},
	}

	selected := selectDependencyObjects("rsnast00", objects, "report")
	if len(selected) != 2 {
		t.Fatalf("expected exact report-name matches, got %#v", selected)
	}
	if !isSupportedDependencyObject(selected[0], "report") && !isSupportedDependencyObject(selected[1], "report") {
		t.Fatalf("expected at least one report object in %#v", selected)
	}
}

func TestDomainsAreRecognizedButNotRemoteFetchable(t *testing.T) {
	domain := AdtObjectRef{
		URI:         "/sap/bc/adt/ddic/domains/boolean",
		Type:        "DOMA/DD",
		Name:        "BOOLEAN",
		PackageName: "SABAPDEMOS",
		Description: "Boolean domain",
	}

	if !isDdicDependencyObject(domain) {
		t.Fatalf("expected domain to be DDIC dependency")
	}
	if inferDdicManifestKind(domain) != "ddic-domain" {
		t.Fatalf("expected ddic-domain kind, got %q", inferDdicManifestKind(domain))
	}
	selected := selectDependencyObjects("boolean", []AdtObjectRef{domain}, "type")
	if len(selected) != 0 {
		t.Fatalf("expected remote domain selection to be unsupported, got %#v", selected)
	}
}

func TestDataElementExactMatchShadowsDomainExactMatch(t *testing.T) {
	objects := []AdtObjectRef{
		{
			URI:         "/sap/bc/adt/ddic/domains/boolean",
			Type:        "DOMA/DD",
			Name:        "BOOLEAN",
			PackageName: "SABAPDEMOS",
			Description: "Boolean domain",
		},
		{
			URI:         "/sap/bc/adt/ddic/dataelements/boolean",
			Type:        "DTEL/DE",
			Name:        "BOOLEAN",
			PackageName: "SABAPDEMOS",
			Description: "Boolean data element",
		},
	}

	selected := selectDependencyObjects("boolean", objects, "type")
	if len(selected) != 1 || selected[0].Type != "DTEL/DE" {
		t.Fatalf("expected data element to shadow exact domain, got %#v", selected)
	}
}
