/*
 *  self-ml.c
 *  self-ml
 *
 *  Created by Alex Gordon on 31/07/2010.
 *  Copyright 2010 __MyCompanyName__. All rights reserved.
 *
 */

#include "self-ml.h"



#pragma mark Declarations

const SFNodeRef SFNullNode = NULL;

typedef struct SFNode_ {
	
	SFNodeRef parent;
	
	SFNodeRef next;
	SFNodeRef firstChild;
	
	SFNodeType nodeType;
	
	//If this is a string node then `stringValue` is the value of the node.
	//If this is a list node then `stringValue` is the head of the node.
	const char* stringValue;
	
} SFNode;


static SFNode* SFNodeForRef(SFNodeRef ref);

static void parseRoot(const char* sourceString, SFNodeRef rootNode);
static void parseNodes(const char* sourceString, size_t length, SFNodeRef node, size_t *offset);
static void parseBlockComment(const char* sourceString, size_t length, SFNodeRef node, size_t *offset);
static void parseLineComment(const char* sourceString, size_t length, SFNodeRef node, size_t *offset);
static void parseCurlyBracketBlock(const char* sourceString, size_t length, SFNodeRef node, size_t *offset);
static const char* parseVerbatimString(const char* sourceString, size_t length, SFNodeRef node, size_t *offset);
static const char* parseBracketString(const char* sourceString, size_t length, SFNodeRef node, size_t *offset);
static const char* parseBacktickString(const char* sourceString, size_t length, SFNodeRef node, size_t *offset);
static void parseList(const char* sourceString, size_t length, SFNodeRef parentNode, size_t *offset);



#pragma mark Creation and Deletion

SFNodeRef SFNodeCreate()
{
	//Allocate a new node
	SFNodeRef node = (SFNodeRef)calloc(1, sizeof(SFNodeRef));
	
	//If allocation failed, return selfml_null
	if (node == NULL)
		return SFNullNode;
	
	return node;
}

SFNodeRef SFNodeCreateFromString(const char* sourceString)
{
	//Allocate a new node
	SFNodeRef node = SFNodeCreate();
	
	//If allocation failed, return selfml_null
	if (node == SFNullNode)
		return SFNullNode;
	
	parseRoot(sourceString, node);
	
	return node;
}

//Recursively free the subtree of `nodeRef`
void SFNodeFree(SFNodeRef nodeRef)
{
	if (nodeRef == SFNullNode)
		return;
	
	if (SFNodeStringValue(nodeRef))
		free((void *)SFNodeStringValue(nodeRef));
	
	if (SFNodeHead(nodeRef))
		free((void *)SFNodeHead(nodeRef));
	
	SFNodeFree(SFNodeNextInList(nodeRef));
	SFNodeFree(SFNodeFirstChild(nodeRef));
	
	free(SFNodeForRef(nodeRef));
}



#pragma mark Parsing

void parseRoot(const char* sourceString, SFNodeRef rootNode)
{
	size_t offset = 0;
	parseNodes(sourceString, strlen(sourceString), rootNode, &offset);
}
void parseNodes(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{	
	for(; *offset < length; (*offset)++) {
		char c = sourceString[*offset];

		if (isspace(c) || c == ']' || c == '}')
		{
			continue;
		}
		else if (c == '{')
		{
			char next_c = '\0';
			if (*offset + 1 < length)
				next_c = sourceString[*offset];
		
			if (next_c == '#')
			{
				parseBlockComment(sourceString, length, node, offset);
			}
			else
			{
				// { ... } is an error!
				// Parse and ignore
				parseCurlyBracketBlock(sourceString, length, node, offset);
			}
		}
		
		else if (c == '#')
		{
			parseLineComment(sourceString, length, node, offset);
		}
		else if (c == '[')
		{
			const char *str = parseBracketString(sourceString, length, node, offset);
			SFNodeAddString(node, str);
		}
		else if (c == '`')
		{
			const char *str = parseBacktickString(sourceString, length, node, offset);
			SFNodeAddString(node, str);
		}
		else if (c == '(')
		{
			parseList(sourceString, length, node, offset);
		}
		else if (c == ')')
		{
			return;
		}
		else
		{
			const char *str = parseVerbatimString(sourceString, length, node, offset);
			SFNodeAddString(node, str);
		}
	}
	
	(*offset)--;
}


// blockComment := '{#' (blockComment | any)+ '#}'
void parseBlockComment(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{
	if (*offset >= length)
		return;
	
	for(; *offset < length; (*offset)++) {
		char c = sourceString[*offset];
		char next_c = '\0';
		if (*offset + 1 < length)
			next_c = sourceString[*offset];
		
		if (c == '{' && next_c == '#')
		{
			parseBlockComment(sourceString, length, node, offset);
		}
		else if (c == '#' && next_c == '}')
		{
			return;
		}
	}
	
	(*offset)--;
}

// lineComment := '#' any+ (CR | LF)
void parseLineComment(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{
	if (*offset >= length)
		return;
	
	for((*offset)++; *offset < length; (*offset)++) {
	
		char c = sourceString[*offset];
		if (c == 0x0A || c == 0x0D)
		{
			return;
		}
	}
	
	(*offset)--;
}

// curlyBracketBlock := '{' (curlyBracketBlock | any)+ '}'
void parseCurlyBracketBlock(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{
	if (*offset >= length)
		return;
	
	for((*offset)++; *offset < length; (*offset)++) {
		char c = sourceString[*offset];
		
		if (c == '{')
		{
			parseCurlyBracketBlock(sourceString, length, node, offset);
		}
		else if (c == '}')
		{
			return;
		}
	}
	
	(*offset)--;
}

// verbatim := [^\s()[\]{}]+
const char* parseVerbatimString(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{
	if (*offset >= length)
		return NULL;
	
	int nestingLevel = 0;
		
	//Pass 1: Find out the length of the string
	size_t i = 0;
	size_t literalLength = 0;
	for(; *offset + i < length; i++) {
		char c = sourceString[i];
		
		if (c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}')
			break;
		
		literalLength++;
	}
	
	//Allocate `literalLength` of memory
	char *buffer = (char *)malloc(literalLength);
	
	//Pass 2: Set the characters in `buffer`
	for((*offset)++, i = 0; *offset < length && i < literalLength; (*offset)++, i++) {
		buffer[i] = sourceString[*offset];
	}
	
	(*offset)--;
	
	return buffer;
}

// bracketString := '[' (bracketString | any)+ ']'
const char* parseBracketString(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{
	if (*offset >= length)
		return NULL;
	
	int nestingLevel = 0;
		
	//Pass 1: Find out the length of the string
	size_t i = 0;
	size_t literalLength = 0;
	for(; *offset + i < length; i++) {
		char c = sourceString[i];
		
		if (c == '[')
			nestingLevel++;
		else if (c == ']')
			nestingLevel--;
			
		if (nestingLevel == -1)
			break;
		
		literalLength++;
	}
	
	//Allocate `literalLength` of memory
	char *buffer = (char *)malloc(literalLength);
	
	//Pass 2: Set the characters in `buffer`
	for((*offset)++, i = 0; *offset < length && i < literalLength; (*offset)++, i++) {
		buffer[i] = sourceString[*offset];
	}
	
	(*offset)--;
	
	return buffer;
}

// backtickString := '`' ('``' | any)+ '`'
const char* parseBacktickString(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{
	if (*offset >= length)
		return NULL;
	
	int nestingLevel = 0;
		
	//Pass 1: Find out the length of the string
	size_t i = 0;
	size_t literalLength = 0;
	for(; *offset + i < length; i++) {
		char c = sourceString[i];
		char next_c = '\0';
		if (i + 1 < length)
			next_c = sourceString[*offset];
		
		if (c == '`')
		{
			if (next_c == '`')
				i++;
			else
				i++;
		}
		
		literalLength++;
	}
	
	//Allocate `literalLength` of memory
	char *buffer = (char *)malloc(literalLength);
	
	//Pass 2: Set the characters in `buffer`
	for((*offset)++, i = 0; *offset < length && i < literalLength; (*offset)++, i++) {
		buffer[i] = sourceString[*offset];
	}
	
	(*offset)--;
	
	return buffer;
}

// list := '(' head nodes ')'
void parseList(const char* sourceString, size_t length, SFNodeRef parentNode, size_t *offset)
{
	if (*offset >= length)
		return;
	
	(*offset)++;
	
	//Create the node
	SFNodeRef node = SFNodeCreate();
	
	//Add to its parent
	SFNodeSetParent(node, parentNode);
	SFNodeAddChild(parentNode, node);
	
	//Head
	const char* head = parseVerbatimString(sourceString, length, parentNode, offset);
	SFNodeSetHead(node, head);
	
	//Children
	parseNodes(sourceString, length, node, offset);
}



#pragma mark Properties

SFNode* SFNodeForRef(SFNodeRef ref)
{
	return (SFNode*)ref;
}

SFNodeType SFNodeGetType(SFNodeRef node)
{
	return SFNodeForRef(node)->nodeType;
}
void SFNodeSetType(SFNodeRef node, SFNodeType newType)
{
	SFNodeForRef(node)->nodeType = newType;
}

SFNodeRef SFNodeParent(SFNodeRef node)
{
	return SFNodeForRef(node)->parent;
}
void SFNodeSetParent(SFNodeRef node, SFNodeRef parent)
{
	SFNodeForRef(node)->parent = parent;
}

SFNodeRef SFNodeFirstChild(SFNodeRef parent)
{
	return SFNodeForRef(parent)->firstChild;
}
void SFNodeSetFirstChild(SFNodeRef parent, SFNodeRef child)
{
	SFNodeForRef(parent)->firstChild = child;
}

SFNodeRef SFNodeNextInList(SFNodeRef node)
{
	return SFNodeForRef(node)->next;
}
//Beware! This function simply sets the next pointer, it doesn't attempt to do any splicing
void SFNodeSetNextInList(SFNodeRef node, SFNodeRef nextNode)
{
	SFNodeForRef(node)->next = nextNode;
}

const char* SFNodeHead(SFNodeRef node)
{
	if (SFNodeGetType(node) == SFNodeTypeList)
		return SFNodeForRef(node)->stringValue;
	return NULL;
}
void SFNodeSetHead(SFNodeRef node, const char *head)
{
	if (SFNodeGetType(node) == SFNodeTypeList)
		SFNodeForRef(node)->stringValue = head;
}

void SFNodeAddString(SFNodeRef parent, const char* str)
{
	SFNodeRef node = SFNodeCreate();
	if (node == SFNullNode)
		return;
	
	SFNodeSetType(node, SFNodeTypeString);
	SFNodeSetStringValue(node, str);
	
	SFNodeAddChild(parent, node);
}
void SFNodeAddChild(SFNodeRef parent, SFNodeRef node)
{
	//If the parent isn't a list, then it can't have any children. Ignore the request to add a child.
	if (SFNodeGetType(parent) != SFNodeTypeList)
		return;
	
	//No, a node cannot be its own parent. Such matters are frowned upon
	if (parent == node)
		return;
	
	SFNodeRef firstChild = SFNodeFirstChild(parent);
	
	//If the parent has no first child, then set it to the new node
	if (firstChild == SFNullNode)
		SFNodeSetFirstChild(parent, node);
	
	//Otherwise find its last child
	SFNodeRef currentNode = firstChild;
	while (currentNode != SFNullNode)
	{
		if (currentNode == node)
			return;
		
		if (SFNodeNextInList(currentNode) == SFNullNode)
		{
			SFNodeSetNextInList(currentNode, node);
		}
	}
}

size_t SFNodeStringValueLength(SFNodeRef node)
{
	if (SFNodeGetType(node) == SFNodeTypeList)
	{
		const char *stringValue = SFNodeStringValue(node);
		
		if (stringValue != NULL)
			return strlen(stringValue);
	}
	
	return 0;
}
const char* SFNodeStringValue(SFNodeRef node)
{
	if (SFNodeGetType(node) == SFNodeTypeString)
		return SFNodeForRef(node)->stringValue;
	return NULL;
}
void SFNodeSetStringValue(SFNodeRef node, const char *str)
{
	if (SFNodeGetType(node) == SFNodeTypeString)
		SFNodeForRef(node)->stringValue = str;
}

//Copies the string representation of `node` into `stringDestination`.
//SFNodeCopyStringValueTo() does NOT append a NULL character.
//`stringDestination` must be large enough to hold `SFNodeStringValueLength(node)` characters.
_Bool SFNodeCopyStringValueTo(SFNodeRef node, const char* stringDestination)
{	
	if (SFNodeStringValueLength(node) == 0)
		return false;
	
	memcpy((void *)stringDestination, (void *)SFNodeStringValue(node), SFNodeStringValueLength(node));
	return true;
}


#pragma mark Representations

size_t SFNodeRepresentationLength(SFNodeRef node)
{
	return 0;
}
_Bool SFNodeRepresentation(SFNodeRef node, const char* stringDestination)
{
	return false;
}

