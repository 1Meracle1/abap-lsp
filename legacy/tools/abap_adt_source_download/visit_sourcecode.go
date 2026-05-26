package main

import (
	"log"
	"path/filepath"
)

func visitSourceCodeLibrary(
	ctx *SapContext,
	packageName string,
	encodedPackageName string,
	objectType AdtObjectTypeInfo,
	basePath string,
) {
	defer ctx.wg.Done()

	objectsFolderPath := filepath.Join(basePath, objectType.ObjectTypeLabel)
	createDirIfNotExists(objectsFolderPath)

	nodeKeys := []string{objectType.NodeId}
	nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedPackageName, "DEVC%2FK", nodeKeys)
	if err != nil {
		log.Printf(
			"failed to fetch %s %s node structure for package %q: %v",
			objectType.CategoryTag,
			objectType.ObjectType,
			packageName,
			err,
		)
		return
	}

	for _, objectInfoNode := range nodeStructure.TreeContent {
		switch objectInfoNode.ObjectType {
		case "PROG/P":
			visitReport(ctx, objectInfoNode, objectsFolderPath)
		case "PROG/I":
			visitInclude(ctx, objectInfoNode, objectsFolderPath)
		case "CLAS/OC":
			visitClass(ctx, objectInfoNode, objectsFolderPath)
		case "FUGR/F":
			visitFunctionGroup(ctx, objectInfoNode, objectsFolderPath)
		case "INTF/OI":
			visitInteface(ctx, objectInfoNode, objectsFolderPath)
		}
	}
}
