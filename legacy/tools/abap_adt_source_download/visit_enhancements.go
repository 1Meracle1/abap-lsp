package main

import (
	"log"
	"path"
)

func visitEnhancements(
	ctx *SapContext,
	packageName string,
	encodedPackageName string,
	objectType AdtObjectTypeInfo,
	basePath string,
) {
	objectsFolderPath := path.Join(basePath, objectType.ObjectTypeLabel)
	createDirIfNotExists(objectsFolderPath)

	nodeKeys := []string{objectType.NodeId}
	nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedPackageName, "DEVC%2FK", nodeKeys)
	if err != nil {
		log.Printf("failed to fetch %s %s node structure for package '%s', err: %v\n", objectType.CategoryTag, objectType.ObjectType, packageName, err)
		return
	}

	for _, objectInfoNode := range nodeStructure.TreeContent {
		switch objectInfoNode.ObjectType {
		case "ENHO/XH":
			visitEnhancement(ctx, objectInfoNode, objectsFolderPath)
		}
	}
}

func visitEnhancement(
	ctx *SapContext,
	info AdtObjectNode,
	basePath string,
) {
	log.Printf("visited enhancement %s\n", info.ObjectName)
	encodedName := encodeObjectName(info.ObjectName)

	folderPath := path.Join(basePath, encodedName)
	createDirIfNotExists(folderPath)

	if info.Expandable == "X" {
		encodedObjectName := encodeObjectName(info.ObjectName)
		encodedObjectType := encodeObjectName(info.ObjectType)
		nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nil)
		if err != nil {
			log.Printf("failed to fetch enhancement %s node structure, err: %v\n", info.ObjectName, err)
			return
		}

		for _, objectType := range nodeStructure.ObjectTypes {
			objectsFolderPath := path.Join(folderPath, objectType.ObjectTypeLabel)
			createDirIfNotExists(objectsFolderPath)

			switch objectType.ObjectType {
			case "ENHS/XB":
				encodedObjectType := encodeObjectName(objectType.ObjectType)
				nodeKeys := []string{objectType.NodeId}
				nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nodeKeys)
				if err != nil {
					log.Printf("failed to fetch %s node structure, err: %v\n", objectType, err)
					continue
				}
				for _, objectInfo := range nodeStructure.TreeContent {
					if objectInfo.ObjectType == objectType.ObjectType {
						visitFunctionModule(ctx, objectInfo, objectsFolderPath, encodedName)
					}
				}
			case "ENHS/XS":
				encodedObjectType := encodeObjectName(objectType.ObjectType)
				nodeKeys := []string{objectType.NodeId}
				nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nodeKeys)
				if err != nil {
					log.Printf("failed to fetch %s node structure, err: %v\n", objectType, err)
					continue
				}
				for _, objectInfoNode := range nodeStructure.TreeContent {
					if objectInfoNode.ObjectType == objectType.ObjectType {
						visitInclude(ctx, objectInfoNode, objectsFolderPath)
					}
				}
			}
		}
	}
}

// func visitBadi(
// 	ctx SapContext,
// 	info AdtObjectNode,
// 	basePath string,
// ) {
// 	log.Printf("visited badi %s\n", info.ObjectName)
// 	encodedName := encodeObjectName(info.ObjectName)

// 	folderPath := path.Join(basePath, encodedName)
// 	createDirIfNotExists(folderPath)

// 	if info.Expandable == "X" {
// 		encodedObjectName := encodeObjectName(info.ObjectName)
// 		encodedObjectType := encodeObjectName(info.ObjectType)
// 		nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nil)
// 		if err != nil {
// 			log.Panicf("failed to fetch badi %s node structure, err: %v", info.ObjectName, err)
// 		}

// 		for _, objectInfoNode := range nodeStructure.ObjectTypes {
// 			objectsFolderPath := path.Join(folderPath, objectInfoNode.ObjectTypeLabel)
// 			createDirIfNotExists(objectsFolderPath)

// 			objectType := objectInfoNode.ObjectType
// 			switch objectType {
// 			case "ENHS/XB":
// 				nodeKeys := []string{objectInfoNode.NodeId}
// 				nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nodeKeys)
// 				if err != nil {
// 					log.Panicf("failed to fetch %s node structure, err: %v", objectType, err)
// 				}
// 				for _, objectInfoNode := range nodeStructure.TreeContent {
// 					if objectInfoNode.ObjectType == objectType {
// 						visitFunctionModule(ctx, objectInfoNode, objectsFolderPath, encodedName)
// 					}
// 				}
// 			case "ENHS/XS":
// 				nodeKeys := []string{objectInfoNode.NodeId}
// 				nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedObjectName, encodedObjectType, nodeKeys)
// 				if err != nil {
// 					log.Panicf("failed to fetch %s node structure, err: %v", objectType, err)
// 				}
// 				for _, objectInfoNode := range nodeStructure.TreeContent {
// 					if objectInfoNode.ObjectType == objectType {
// 						visitInclude(ctx, objectInfoNode, objectsFolderPath)
// 					}
// 				}
// 			}
// 		}
// 	}
// }
