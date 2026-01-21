package main

import "core:fmt"
import "core:strings"

@(private="file")
escape_error :: proc(e: string) -> string {
    return fmt.tprintf("invalid URL escape '", e, "'")
}
@(private="file")
invalid_host_error :: proc(e: string) -> string {
    return fmt.tprintf("invalid character '", e, "' in host name")
}

Encode_Kind :: enum {
	Path,
	PathSegment,
	Host,
	Zone,
	UserPassword,
	QueryComponent,
	Fragment,
}

// does the inverse transformation of [QueryEscape],
// converting each 3-byte encoded substring of the form "%AB" into the
// hex-decoded byte 0xAB.
// It returns an error if any % is not followed by two hexadecimal
// digits.
path_unescape :: proc(s: string) -> (res: string, ok: bool, err: string) {
	return unescape(s, .PathSegment)
}

// unescapes a string; the mode specifies
// which section of the URL string is being unescaped.
unescape :: proc(s: string, mode: Encode_Kind) -> (res: string, ok: bool, err: string) {
    s := s
	// Count %, check that they're well-formed.
	n := 0
	hasPlus := false
	for i := 0; i < len(s); {
		switch s[i] {
		case '%':
			n += 1
			if i+2 >= len(s) || !ishex(s[i+1]) || !ishex(s[i+2]) {
				s = s[i:]
				if len(s) > 3 {
					s = s[:3]
				}
				return "", false, escape_error(s)
			}
			// Per https://tools.ietf.org/html/rfc3986#page-21
			// in the host component %-encoding can only be used
			// for non-ASCII bytes.
			// But https://tools.ietf.org/html/rfc6874#section-2
			// introduces %25 being allowed to escape a percent sign
			// in IPv6 scoped-address literals. Yay.
			if mode == .Host && unhex(s[i+1]) < 8 && s[i:i+3] != "%25" {
				return "", false, escape_error(s[i : i+3])
			}
			if mode == .Zone {
				// RFC 6874 says basically "anything goes" for zone identifiers
				// and that even non-ASCII can be redundantly escaped,
				// but it seems prudent to restrict %-escaped bytes here to those
				// that are valid host name bytes in their unescaped form.
				// That is, you can use escaping in the zone identifier but not
				// to introduce bytes you couldn't just write directly.
				// But Windows puts spaces here! Yay.
				v := unhex(s[i+1])<<4 | unhex(s[i+2])
				if s[i:i+3] != "%25" && v != ' ' && should_escape(v, .Host) {
					return "", false, escape_error(s[i : i+3])
				}
			}
			i += 3
		case '+':
			hasPlus = mode == .QueryComponent
			i += 1
		case:
			if (mode == .Host || mode == .Zone) && s[i] < 0x80 && should_escape(s[i], mode) {
				return "", false, invalid_host_error(s[i : i+1])
			}
			i += 1
		}
	}

	if n == 0 && !hasPlus {
		return s, true, ""
	}

	t: strings.Builder
    strings.builder_init_len_cap(&t, 0, len(s) - 2*n)
	for i := 0; i < len(s); i += 1 {
		switch s[i] {
		case '%':
            strings.write_byte(&t, unhex(s[i+1])<<4 | unhex(s[i+2]))
			i += 2
		case '+':
			if mode == .QueryComponent {
				strings.write_byte(&t, ' ')
			} else {
				strings.write_byte(&t, '+')
			}
		case:
			strings.write_byte(&t, s[i])
		}
	}
	return strings.to_string(t), true, ""
}

// escapes the string so it can be safely placed inside a [URL] path segment,
// replacing special characters (including /) with %XX sequences as needed.
path_escape :: proc(s: string) -> string {
	return escape(s, .PathSegment)
}

escape :: proc(s: string, mode: Encode_Kind) -> string {
	spaceCount, hexCount := 0, 0
	for i := 0; i < len(s); i += 1 {
		c := s[i]
		if should_escape(c, mode) {
			if c == ' ' && mode == .QueryComponent {
				spaceCount += 1
			} else {
				hexCount += 1
			}
		}
	}

	if spaceCount == 0 && hexCount == 0 {
		return s
	}

	buf: [64]byte
	t: []byte

	required := len(s) + 2*hexCount
	if required <= len(buf) {
		t = buf[:required]
	} else {
		t = make([]byte, required)
	}

	if hexCount == 0 {
		copy(t, s)
		for i := 0; i < len(s); i += 1 {
			if s[i] == ' ' {
				t[i] = '+'
			}
		}
		return string(t)
	}

    upperhex := UPPERHEX
	j := 0
	for i := 0; i < len(s); i += 1 {
		switch c := s[i]; {
		case c == ' ' && mode == .QueryComponent:
			t[j] = '+'
			j += 1
		case should_escape(c, mode):
			t[j] = '%'
			t[j+1] = upperhex[c>>4]
			t[j+2] = upperhex[c&15]
			j += 3
		case:
			t[j] = s[i]
			j += 1
		}
	}
	return string(t)
}

// Return true if the specified character should be escaped when
// appearing in a URL string, according to RFC 3986.
//
// Please be informed that for now shouldEscape does not check all
// reserved characters correctly. See golang.org/issue/5684.
should_escape :: proc(c: byte, mode: Encode_Kind) -> bool {
	// §2.3 Unreserved characters (alphanum)
	if 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || '0' <= c && c <= '9' {
		return false
	}

	if mode == .Host || mode == .Zone {
		// §3.2.2 Host allows
		//	sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
		// as part of reg-name.
		// We add : because we include :port as part of host.
		// We add [ ] because we include [ipv6]:port as part of host.
		// We add < > because they're the only characters left that
		// we could possibly allow, and Parse will reject them if we
		// escape them (because hosts can't use %-encoding for
		// ASCII bytes).
		switch c {
		case '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=', ':', '[', ']', '<', '>', '"':
			return false
		}
	}

	switch c {
	case '-', '_', '.', '~': // §2.3 Unreserved characters (mark)
		return false

	case '$', '&', '+', ',', '/', ':', ';', '=', '?', '@': // §2.2 Reserved characters (reserved)
		// Different sections of the URL allow a few of
		// the reserved characters to appear unescaped.
		#partial switch mode {
		case .Path: // §3.3
			// The RFC allows : @ & = + $ but saves / ; , for assigning
			// meaning to individual path segments. This package
			// only manipulates the path as a whole, so we allow those
			// last three as well. That leaves only ? to escape.
			return c == '?'

		case .PathSegment: // §3.3
			// The RFC allows : @ & = + $ but saves / ; , for assigning
			// meaning to individual path segments.
			return c == '/' || c == ';' || c == ',' || c == '?'

		case .UserPassword: // §3.2.1
			// The RFC allows ';', ':', '&', '=', '+', '$', and ',' in
			// userinfo, so we must escape only '@', '/', and '?'.
			// The parsing of userinfo treats ':' as special so we must escape
			// that too.
			return c == '@' || c == '/' || c == '?' || c == ':'

		case .QueryComponent: // §3.4
			// The RFC reserves (so we must escape) everything.
			return true

		case .Fragment: // §4.1
			// The RFC text is silent but the grammar allows
			// everything, so escape nothing.
			return false
		}
	}

	if mode == .Fragment {
		// RFC 3986 §2.2 allows not escaping sub-delims. A subset of sub-delims are
		// included in reserved from RFC 2396 §2.2. The remaining sub-delims do not
		// need to be escaped. To minimize potential breakage, we apply two restrictions:
		// (1) we always escape sub-delims outside of the fragment, and (2) we always
		// escape single quote to avoid breaking callers that had previously assumed that
		// single quotes would be escaped. See issue #19917.
		switch c {
		case '!', '(', ')', '*':
			return false
		}
	}

	// Everything else must be escaped.
	return true
}

UPPERHEX :: "0123456789ABCDEF"

ishex :: proc(c: byte) -> bool {
	switch {
	case '0' <= c && c <= '9':
		return true
	case 'a' <= c && c <= 'f':
		return true
	case 'A' <= c && c <= 'F':
		return true
	}
	return false
}

unhex :: proc(c: byte) -> byte {
	switch {
	case '0' <= c && c <= '9':
		return c - '0'
	case 'a' <= c && c <= 'f':
		return c - 'a' + 10
	case 'A' <= c && c <= 'F':
		return c - 'A' + 10
	case:
		panic("invalid hex character")
	}
}