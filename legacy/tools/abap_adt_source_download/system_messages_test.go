package main

import (
	"strings"
	"testing"
)

func TestSessionBootstrapAcceptAdvertisesAtomFeedAndXML(t *testing.T) {
	if !strings.Contains(sessionBootstrapAccept, "application/atom+xml;type=feed") {
		t.Fatalf("expected atom feed media type in %q", sessionBootstrapAccept)
	}
	if !strings.Contains(sessionBootstrapAccept, "application/xml") {
		t.Fatalf("expected xml media type in %q", sessionBootstrapAccept)
	}
}
