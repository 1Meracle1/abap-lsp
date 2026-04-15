package main

import (
	"log"
	"os"
	"path/filepath"
)

func visitClass(ctx *SapContext, info AdtObjectNode, basePath string) {
	encodedClassName := encodeObjectName(info.ObjectName)
	classPath := filepath.Join(basePath, encodedClassName+".abap")
	if fileExists(classPath) {
		return
	}
	log.Printf("visited class %s", info.ObjectName)

	classSource, err := fetchClassSource(ctx, encodedClassName)
	if err != nil {
		log.Printf("failed to fetch class %s source code: %v", info.ObjectName, err)
		return
	}

	err = os.WriteFile(classPath, []byte(classSource), os.ModePerm)
	if err != nil {
		log.Panicf("failed to write class file %s: %v", classPath, err)
	}
}

func visitInteface(ctx *SapContext, info AdtObjectNode, basePath string) {
	encodedName := encodeObjectName(info.ObjectName)
	filePath := filepath.Join(basePath, encodedName+".abap")
	if fileExists(filePath) {
		return
	}
	log.Printf("visited interface %s", info.ObjectName)

	source, err := fetchInterfaceSource(ctx, encodedName)
	if err != nil {
		log.Printf("failed to fetch interface %s source code: %v", info.ObjectName, err)
		return
	}

	err = os.WriteFile(filePath, []byte(source), os.ModePerm)
	if err != nil {
		log.Panicf("failed to write interface file %s: %v", filePath, err)
	}
}
