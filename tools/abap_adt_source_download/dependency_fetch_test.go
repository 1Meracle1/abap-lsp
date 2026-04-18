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
