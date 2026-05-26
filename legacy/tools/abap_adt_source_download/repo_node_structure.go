package main

import (
	"bytes"
	"encoding/xml"
	"fmt"
	"io"
	"log"
	"net/http"
	"strings"
)

type RepositoryNodeStructure struct {
	TreeContent []AdtObjectNode         `xml:"TREE_CONTENT>SEU_ADT_REPOSITORY_OBJ_NODE"`
	Categories  []AdtObjectCategoryInfo `xml:"CATEGORIES>SEU_ADT_OBJECT_CATEGORY_INFO"`
	ObjectTypes []AdtObjectTypeInfo     `xml:"OBJECT_TYPES>SEU_ADT_OBJECT_TYPE_INFO"`
}

type AdtObjectNode struct {
	ObjectType   string `xml:"OBJECT_TYPE"`
	ObjectName   string `xml:"OBJECT_NAME"`
	ObjectUri    string `xml:"OBJECT_URI"`
	ObjectVitUri string `xml:"OBJECT_VIT_URI"`
	Expandable   string `xml:"EXPANDABLE"`
}

type AdtObjectCategoryInfo struct {
	Category      string `xml:"CATEGORY"`
	CategoryLabel string `xml:"CATEGORY_LABEL"`
}

type AdtObjectTypeInfo struct {
	ObjectType      string `xml:"OBJECT_TYPE"`
	CategoryTag     string `xml:"CATEGORY_TAG"`
	ObjectTypeLabel string `xml:"OBJECT_TYPE_LABEL"`
	NodeId          string `xml:"NODE_ID"`
}

func fetchRepositoryNodeStructure(
	ctx *SapContext,
	encodedName string,
	parentType string,
	nodeKeys []string,
) (nodeStructure RepositoryNodeStructure, err error) {
	var bodyBuilder strings.Builder
	bodyBuilder.WriteString(`<?xml version="1.0" encoding="UTF-8" ?> 
<asx:abap version="1.0" xmlns:asx="http://www.sap.com/abapxml">
<asx:values>
<DATA>
`)
	if len(nodeKeys) == 0 {
		bodyBuilder.WriteString("<TV_NODEKEY>000000</TV_NODEKEY>")
	} else {
		for _, nodeKey := range nodeKeys {
			bodyBuilder.WriteString("<TV_NODEKEY>")
			bodyBuilder.WriteString(nodeKey)
			bodyBuilder.WriteString("</TV_NODEKEY>\n")
		}
	}
	bodyBuilder.WriteString(`
</DATA>
</asx:values>
</asx:abap>`)

	rawURL := ctx.adtURL("/repository/nodestructure?parent_name=" + encodedName + "&parent_tech_name=" + encodedName + "&parent_type=" + parentType + "&withShortDescriptions=true")
	req, err := http.NewRequest("POST", rawURL, bytes.NewBuffer([]byte(bodyBuilder.String())))
	if err != nil {
		return nodeStructure, err
	}

	headers := map[string]string{
		"Content-Type":        "application/vnd.sap.as+xml; charset=UTF-8; dataname=null",
		"Accept":              "application/vnd.sap.as+xml;charset=UTF-8;dataname=com.sap.adt.RepositoryObjectTreeContent",
		"X-sap-adt-profiling": "server-time",
		"Cache-Control":       "no-cache",
		"x-csrf-token":        ctx.csrfToken,
	}

	resp, err := ctx.doRequestWithHeaders(req, headers)
	if err != nil {
		return nodeStructure, err
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nodeStructure, err
	}
	if resp.StatusCode > 299 {
		return nodeStructure, fmt.Errorf("HTTP response code: %d, response body: %q", resp.StatusCode, body)
	}

	nodeStructureResponse := string(body)
	startValuesData := "<asx:values>"
	startIdx := strings.Index(nodeStructureResponse, startValuesData)
	if startIdx == -1 {
		return nodeStructure, nil
	}
	startIdx += len(startValuesData)
	endValuesData := "</asx:values>"
	endIdx := strings.Index(nodeStructureResponse, endValuesData)
	if endIdx == -1 {
		log.Panicf("failed to interpret response (failed to find %s): %s", endValuesData, nodeStructureResponse)
	}
	nodeStructureRaw := nodeStructureResponse[startIdx:endIdx]

	err = xml.Unmarshal([]byte(nodeStructureRaw), &nodeStructure)
	return nodeStructure, err
}
