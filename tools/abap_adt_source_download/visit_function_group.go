package main

import (
	"log"
	"os"
	"path/filepath"
)

func visitFunctionGroup(ctx *SapContext, info AdtObjectNode, basePath string) {
	log.Printf("visited function group %s", info.ObjectName)
	encodedFunctionGroupName := encodeObjectName(info.ObjectName)

	functionGroupFolderPath := filepath.Join(basePath, encodedFunctionGroupName)
	createDirIfNotExists(functionGroupFolderPath)

	functionGroupSource, err := fetchFunctionGroupSource(ctx, encodedFunctionGroupName)
	if err != nil {
		log.Printf("failed to fetch function group %s source code: %v", info.ObjectName, err)
		return
	}

	reportPath := filepath.Join(functionGroupFolderPath, encodedFunctionGroupName+".abap")
	err = os.WriteFile(reportPath, []byte(functionGroupSource), os.ModePerm)
	if err != nil {
		log.Panicf("failed to write function group file %s: %v", reportPath, err)
	}

	if info.Expandable != "X" {
		return
	}

	encodedObjectName := encodeObjectName(info.ObjectName)
	encodedObjectType := encodeObjectName(info.ObjectType)
	nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nil)
	if err != nil {
		log.Printf("failed to fetch function group %s node structure: %v", info.ObjectName, err)
		return
	}

	for _, objectInfoNode := range nodeStructure.ObjectTypes {
		objectsFolderPath := filepath.Join(functionGroupFolderPath, objectInfoNode.ObjectTypeLabel)
		createDirIfNotExists(objectsFolderPath)

		switch objectInfoNode.ObjectType {
		case "FUGR/FF", "FUGR/I":
			nodeKeys := []string{objectInfoNode.NodeId}
			branch, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nodeKeys)
			if err != nil {
				log.Printf("failed to fetch %s node structure: %v", objectInfoNode.ObjectType, err)
				continue
			}
			for _, childNode := range branch.TreeContent {
				switch objectInfoNode.ObjectType {
				case "FUGR/FF":
					if childNode.ObjectType == "FUGR/FF" {
						visitFunctionModule(ctx, childNode, objectsFolderPath, encodedFunctionGroupName)
					}
				case "FUGR/I":
					if childNode.ObjectType == "FUGR/I" {
						visitInclude(ctx, childNode, objectsFolderPath)
					}
				}
			}
		}
	}
}

func visitFunctionModule(
	ctx *SapContext,
	info AdtObjectNode,
	basePath string,
	functionGroupEncodedName string,
) {
	encodedName := encodeObjectName(info.ObjectName)
	filePath := filepath.Join(basePath, encodedName+".abap")
	if fileExists(filePath) {
		return
	}
	log.Printf("visited function module %s", info.ObjectName)

	source, err := fetchFunctionModuleSource(ctx, functionGroupEncodedName, encodedName)
	if err != nil {
		log.Printf("failed to fetch function module %s source code: %v", info.ObjectName, err)
		return
	}

	err = os.WriteFile(filePath, []byte(source), os.ModePerm)
	if err != nil {
		log.Panicf("failed to write function module file %s: %v", filePath, err)
	}
}
