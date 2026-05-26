package main

import (
	"log"
	"path/filepath"
)

func visitPackage(ctx *SapContext, packageName string, basePath string) {
	defer ctx.wg.Done()

	encodedPackageName := encodeObjectName(packageName)
	nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedPackageName, "DEVC%2FK", nil)
	if err != nil {
		log.Printf("failed to fetch node structure for package %q: %v", packageName, err)
		return
	}

	packageFolderPath := filepath.Join(basePath, encodedPackageName)
	createDirIfNotExists(packageFolderPath)

	objectsPaths := make(map[string]string)
	for _, categoryNode := range nodeStructure.Categories {
		objectsPath := filepath.Join(packageFolderPath, categoryNode.CategoryLabel)
		createDirIfNotExists(objectsPath)
		objectsPaths[categoryNode.Category] = objectsPath
	}

	for _, objectType := range nodeStructure.ObjectTypes {
		switch objectType.CategoryTag {
		case "packages":
			for _, node := range nodeStructure.TreeContent {
				if node.ObjectType == objectType.ObjectType {
					ctx.wg.Add(1)
					visitPackage(ctx, node.ObjectName, objectsPaths[objectType.CategoryTag])
				}
			}
		case "dictionary":
			ctx.wg.Add(1)
			go visitDictionary(ctx, packageName, encodedPackageName, objectType, objectsPaths[objectType.CategoryTag])
		case "source_library":
			ctx.wg.Add(1)
			go visitSourceCodeLibrary(ctx, packageName, encodedPackageName, objectType, objectsPaths[objectType.CategoryTag])
		}
	}
}
