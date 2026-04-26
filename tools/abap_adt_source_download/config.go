package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

const (
	defaultRequestsPerMinute = 60
	defaultMaxConcurrent     = 4
	defaultSapClient         = "100"
)

type Config struct {
	BaseURL           string
	Username          string
	Password          string
	SapClient         string
	OutputDir         string
	ObjectsFile       string
	ObjectFilter      *ObjectFilter
	Packages          []string
	RequestsPerMinute int
	MaxConcurrent     int
	CleanOutput       bool
	EnvFile           string
}

type stringSliceFlag []string

func (s *stringSliceFlag) String() string {
	return strings.Join(*s, ",")
}

func (s *stringSliceFlag) Set(value string) error {
	trimmed := strings.TrimSpace(value)
	if trimmed == "" {
		return nil
	}
	*s = append(*s, trimmed)
	return nil
}

func loadConfig(args []string) (Config, error) {
	var cfg Config
	var packages stringSliceFlag

	fs := flag.NewFlagSet("abap_adt_source_download", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)
	fs.StringVar(&cfg.BaseURL, "url", "", "SAP host root or full /sap/bc/adt URL")
	fs.StringVar(&cfg.Username, "user", "", "SAP username")
	fs.StringVar(&cfg.Password, "pass", "", "SAP password")
	fs.StringVar(&cfg.SapClient, "client", "", "SAP client")
	fs.StringVar(&cfg.OutputDir, "output", "", "Export root directory")
	fs.StringVar(&cfg.ObjectsFile, "objects-file", "", "Optional newline-delimited object-name file; only matching objects are downloaded")
	fs.StringVar(&cfg.EnvFile, "env-file", "", "Optional dotenv file path")
	fs.IntVar(&cfg.RequestsPerMinute, "rpm", 0, "Max ADT requests per minute")
	fs.IntVar(&cfg.MaxConcurrent, "parallel", 0, "Max concurrent ADT requests")
	fs.BoolVar(&cfg.CleanOutput, "clean", false, "Delete existing export contents before download")
	fs.Var(&packages, "package", "Package to export; repeatable")
	fs.Usage = func() {
		fmt.Fprintf(fs.Output(), "Usage: %s [options]\n\n", filepath.Base(os.Args[0]))
		fmt.Fprintln(fs.Output(), "Options:")
		fs.PrintDefaults()
		fmt.Fprintln(fs.Output(), "")
		fmt.Fprintln(fs.Output(), "Environment fallback:")
		fmt.Fprintln(fs.Output(), "  ABAP_ADT_URL / ABAP_ADT_BASE_URL / SAPBASE_URL")
		fmt.Fprintln(fs.Output(), "  ABAP_ADT_USER / ABAP_ADT_USERNAME / SAPUSER")
		fmt.Fprintln(fs.Output(), "  ABAP_ADT_PASSWORD / SAPPASS")
		fmt.Fprintln(fs.Output(), "  ABAP_ADT_CLIENT / SAPCLIENT")
		fmt.Fprintln(fs.Output(), "  ABAP_ADT_OUTPUT / OUTPUT_FOLDER")
		fmt.Fprintln(fs.Output(), "  ABAP_ADT_OBJECTS_FILE")
		fmt.Fprintln(fs.Output(), "  ABAP_ADT_PACKAGES")
		fmt.Fprintln(fs.Output(), "  RATE_LIMIT_RPM / MAX_CONCURRENT_REQUESTS")
	}
	if err := fs.Parse(args); err != nil {
		return cfg, err
	}

	dotenvPath := cfg.EnvFile
	if dotenvPath == "" {
		dotenvPath = ".env"
	}
	dotenv, err := readDotenvFile(dotenvPath)
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return cfg, err
	}

	cfg.BaseURL = normalizeBaseURL(firstNonEmpty(
		cfg.BaseURL,
		os.Getenv("ABAP_ADT_URL"),
		os.Getenv("ABAP_ADT_BASE_URL"),
		os.Getenv("SAPBASE_URL"),
		dotenv["ABAP_ADT_URL"],
		dotenv["ABAP_ADT_BASE_URL"],
		dotenv["SAPBASE_URL"],
	))
	cfg.Username = firstNonEmpty(
		cfg.Username,
		os.Getenv("ABAP_ADT_USER"),
		os.Getenv("ABAP_ADT_USERNAME"),
		os.Getenv("SAPUSER"),
		dotenv["ABAP_ADT_USER"],
		dotenv["ABAP_ADT_USERNAME"],
		dotenv["SAPUSER"],
	)
	cfg.Password = firstNonEmpty(
		cfg.Password,
		os.Getenv("ABAP_ADT_PASSWORD"),
		os.Getenv("SAPPASS"),
		dotenv["ABAP_ADT_PASSWORD"],
		dotenv["SAPPASS"],
	)
	cfg.SapClient = firstNonEmpty(
		cfg.SapClient,
		os.Getenv("ABAP_ADT_CLIENT"),
		os.Getenv("SAPCLIENT"),
		dotenv["ABAP_ADT_CLIENT"],
		dotenv["SAPCLIENT"],
		defaultSapClient,
	)
	cfg.OutputDir = firstNonEmpty(
		cfg.OutputDir,
		os.Getenv("ABAP_ADT_OUTPUT"),
		os.Getenv("OUTPUT_FOLDER"),
		dotenv["ABAP_ADT_OUTPUT"],
		dotenv["OUTPUT_FOLDER"],
	)
	if cfg.OutputDir != "" {
		outputDir, err := filepath.Abs(cfg.OutputDir)
		if err != nil {
			return cfg, fmt.Errorf("resolve output dir: %w", err)
		}
		cfg.OutputDir = outputDir
	}
	cfg.ObjectsFile = firstNonEmpty(
		cfg.ObjectsFile,
		os.Getenv("ABAP_ADT_OBJECTS_FILE"),
		dotenv["ABAP_ADT_OBJECTS_FILE"],
	)
	if cfg.ObjectsFile != "" {
		objectsFile, err := filepath.Abs(cfg.ObjectsFile)
		if err != nil {
			return cfg, fmt.Errorf("resolve objects file: %w", err)
		}
		cfg.ObjectsFile = objectsFile
		cfg.ObjectFilter, err = loadObjectFilter(cfg.ObjectsFile)
		if err != nil {
			return cfg, err
		}
	}
	cfg.Packages = dedupeStrings(append(packages, splitPackageValues(firstNonEmpty(
		os.Getenv("ABAP_ADT_PACKAGES"),
		dotenv["ABAP_ADT_PACKAGES"],
	))...))
	cfg.RequestsPerMinute = firstPositiveInt(
		fmt.Sprintf("%d", cfg.RequestsPerMinute),
		os.Getenv("RATE_LIMIT_RPM"),
		dotenv["RATE_LIMIT_RPM"],
		fmt.Sprintf("%d", defaultRequestsPerMinute),
	)
	cfg.MaxConcurrent = firstPositiveInt(
		fmt.Sprintf("%d", cfg.MaxConcurrent),
		os.Getenv("MAX_CONCURRENT_REQUESTS"),
		dotenv["MAX_CONCURRENT_REQUESTS"],
		fmt.Sprintf("%d", defaultMaxConcurrent),
	)

	if cfg.BaseURL == "" {
		return cfg, errors.New("missing SAP URL; set -url or ABAP_ADT_URL")
	}
	if cfg.Username == "" {
		return cfg, errors.New("missing SAP username; set -user or ABAP_ADT_USER")
	}
	if cfg.Password == "" {
		return cfg, errors.New("missing SAP password; set -pass or ABAP_ADT_PASSWORD")
	}
	if cfg.OutputDir == "" {
		return cfg, errors.New("missing output directory; set -output or ABAP_ADT_OUTPUT")
	}
	if len(cfg.Packages) == 0 {
		return cfg, errors.New("no packages configured; pass -package or ABAP_ADT_PACKAGES")
	}

	return cfg, nil
}

func readDotenvFile(path string) (map[string]string, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	values := map[string]string{}
	for _, rawLine := range strings.Split(string(content), "\n") {
		line := strings.TrimSpace(strings.TrimSuffix(rawLine, "\r"))
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		separator := strings.IndexByte(line, '=')
		if separator <= 0 {
			continue
		}

		key := strings.TrimSpace(line[:separator])
		value := strings.TrimSpace(line[separator+1:])
		if key == "" {
			continue
		}

		if (strings.HasPrefix(value, "\"") && strings.HasSuffix(value, "\"")) ||
			(strings.HasPrefix(value, "'") && strings.HasSuffix(value, "'")) {
			value = value[1 : len(value)-1]
		} else if comment := strings.IndexByte(value, '#'); comment >= 0 {
			value = strings.TrimSpace(value[:comment])
		}
		values[key] = value
	}

	return values, nil
}

func normalizeBaseURL(value string) string {
	trimmed := strings.TrimRight(strings.TrimSpace(value), "/")
	if trimmed == "" {
		return ""
	}
	if strings.Contains(strings.ToLower(trimmed), "/sap/bc/adt") {
		return trimmed
	}
	return trimmed + "/sap/bc/adt"
}

func firstNonEmpty(values ...string) string {
	for _, value := range values {
		if trimmed := strings.TrimSpace(value); trimmed != "" {
			return trimmed
		}
	}
	return ""
}

func firstPositiveInt(values ...string) int {
	for _, value := range values {
		trimmed := strings.TrimSpace(value)
		if trimmed == "" {
			continue
		}
		number, err := strconv.Atoi(trimmed)
		if err == nil && number > 0 {
			return number
		}
	}
	return 0
}

func splitPackageValues(value string) []string {
	if strings.TrimSpace(value) == "" {
		return nil
	}
	return strings.FieldsFunc(value, func(r rune) bool {
		return r == ',' || r == ';' || r == '\n' || r == '\r'
	})
}

func dedupeStrings(values []string) []string {
	result := make([]string, 0, len(values))
	seen := make(map[string]struct{}, len(values))
	for _, value := range values {
		trimmed := strings.TrimSpace(value)
		if trimmed == "" {
			continue
		}
		if _, ok := seen[trimmed]; ok {
			continue
		}
		seen[trimmed] = struct{}{}
		result = append(result, trimmed)
	}
	return result
}
