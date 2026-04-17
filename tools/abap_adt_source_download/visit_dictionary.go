package main

import (
	"log"
	"os"
	"path/filepath"
)

func visitDictionary(
	ctx *SapContext,
	packageName string,
	encodedPackageName string,
	objectType AdtObjectTypeInfo,
	basePath string,
) {
	defer ctx.wg.Done()

	nodeKeys := []string{objectType.NodeId}
	nodeStructure, err := fetchRepositoryNodeStructure(ctx, encodedPackageName, "DEVC%2FK", nodeKeys)
	if err != nil {
		log.Panicf(
			"failed to fetch %s %s node structure for package %q: %v",
			objectType.CategoryTag,
			objectType.ObjectType,
			packageName,
			err,
		)
	}

	objectsFolderPath := filepath.Join(basePath, objectType.ObjectTypeLabel)
	createDirIfNotExists(objectsFolderPath)

	for _, objectInfo := range nodeStructure.TreeContent {
		switch objectInfo.ObjectType {
		case "DTEL/DE":
			visitDictDataElement(ctx, objectType, objectInfo, objectsFolderPath)
		case "TABL/DS", "TABL/DT", "TABL/DA", "VIEW/DV", "TTYP/DA":
			visitDictTable(ctx, objectType, objectInfo, objectsFolderPath)
		}
	}
}

func visitDictDataElement(
	ctx *SapContext,
	objectType AdtObjectTypeInfo,
	objectInfo AdtObjectNode,
	basePath string,
) {
	if !ctx.shouldFetchObject(objectInfo.ObjectName) {
		return
	}
	filePath := filepath.Join(basePath, encodeObjectName(objectInfo.ObjectName)+".xml")
	if fileExists(filePath) {
		return
	}
	createDirIfNotExists(basePath)
	log.Printf("visited %s %s", objectType.ObjectTypeLabel, objectInfo.ObjectName)

	src, err := fetchDictDataElement(ctx, encodeObjectName(objectInfo.ObjectName))
	if err != nil {
		log.Printf("failed to fetch dictionary %s: %v", objectType.ObjectTypeLabel, err)
		return
	}

	err = os.WriteFile(filePath, []byte(src), os.ModePerm)
	if err != nil {
		log.Panicf("failed to write dictionary %s file %s: %v", objectType.ObjectTypeLabel, filePath, err)
	}
}

func visitDictTable(
	ctx *SapContext,
	objectType AdtObjectTypeInfo,
	objectInfo AdtObjectNode,
	basePath string,
) {
	if !ctx.shouldFetchObject(objectInfo.ObjectName) {
		return
	}
	filePath := filepath.Join(basePath, encodeObjectName(objectInfo.ObjectName)+".xml")
	if fileExists(filePath) {
		return
	}
	createDirIfNotExists(basePath)
	log.Printf("visited %s %s", objectType.ObjectTypeLabel, objectInfo.ObjectName)

	src, err := fetchDictElementInfo(ctx, encodeObjectName(objectInfo.ObjectName))
	if err != nil {
		log.Printf("failed to fetch dictionary %s: %v", objectType.ObjectTypeLabel, err)
		return
	}

	err = os.WriteFile(filePath, []byte(src), os.ModePerm)
	if err != nil {
		log.Panicf("failed to write dictionary %s file %s: %v", objectType.ObjectTypeLabel, filePath, err)
	}
}
