package main

import (
	"log"
	"os"
	"path/filepath"
)

func visitReport(ctx *SapContext, info AdtObjectNode, basePath string) {
	fetchReport := ctx.shouldFetchObject(info.ObjectName)
	encodedReportName := encodeObjectName(info.ObjectName)
	reportFolderPath := filepath.Join(basePath, encodedReportName)
	if fetchReport {
		log.Printf("visited report %s", info.ObjectName)
		createDirIfNotExists(reportFolderPath)

		reportPath := filepath.Join(reportFolderPath, encodedReportName+".abap")
		if !fileExists(reportPath) {
			reportSource, err := fetchReportSource(ctx, encodedReportName)
			if err != nil {
				log.Printf("failed to fetch report %s source code: %v", info.ObjectName, err)
				return
			}

			err = os.WriteFile(reportPath, []byte(reportSource), os.ModePerm)
			if err != nil {
				log.Panicf("failed to write report file %s: %v", reportPath, err)
			}
		}
	}

	if info.Expandable != "X" {
		return
	}

	encodedObjectName := encodeObjectName(info.ObjectName)
	encodedObjectType := encodeObjectName(info.ObjectType)
	nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nil)
	if err != nil {
		log.Printf("failed to fetch report %s node structure: %v", info.ObjectName, err)
		return
	}

	for _, objectInfoNode := range nodeStructure.ObjectTypes {
		if objectInfoNode.ObjectType != "PROG/I" {
			continue
		}

		includesFolderPath := filepath.Join(reportFolderPath, objectInfoNode.ObjectTypeLabel)
		createDirIfNotExists(includesFolderPath)

		nodeKeys := []string{objectInfoNode.NodeId}
		includeStructure, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nodeKeys)
		if err != nil {
			log.Printf("failed to fetch %s node structure: %v", objectInfoNode.ObjectType, err)
			continue
		}

		for _, includeNode := range includeStructure.TreeContent {
			if includeNode.ObjectType == "PROG/I" {
				visitInclude(ctx, includeNode, includesFolderPath)
			}
		}
	}
}

func visitInclude(ctx *SapContext, info AdtObjectNode, basePath string) {
	if !ctx.shouldFetchObject(info.ObjectName) {
		return
	}
	encodedIncludeName := encodeObjectName(info.ObjectName)
	includePath := filepath.Join(basePath, encodedIncludeName+".abap")
	if fileExists(includePath) {
		return
	}
	createDirIfNotExists(basePath)
	log.Printf("visited include %s", info.ObjectName)

	includeSource, err := fetchIncludeSource(ctx, encodedIncludeName)
	if err != nil {
		log.Printf("failed to fetch include %s source code: %v", info.ObjectName, err)
		return
	}

	err = os.WriteFile(includePath, []byte(includeSource), os.ModePerm)
	if err != nil {
		log.Panicf("failed to write include file %s: %v", includePath, err)
	}
}
