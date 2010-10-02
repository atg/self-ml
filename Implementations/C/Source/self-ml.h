/*
 *  self-ml.h
 *  self-ml
 *  
 *	Created by Alex Gordon.
 *  For more information, see http://dequechair.com/self-ml
 *  
 *  I, the copyright holder of this work, hereby release it into the public domain. This applies worldwide.
 *  In case this is not legally possible,
 *  I grant any entity the right to use this work for any purpose, without any conditions, unless such conditions are required by law.
 *  
 *  If you have an questions about the above declaration, please email anythingfileability.net
 */

#pragma once

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>


struct SFNode_;
typedef struct SFNode_* SFNodeRef;

extern const SFNodeRef SFNullNode;

typedef enum {

	SFNodeTypeList = 0,
	SFNodeTypeString
	
} SFNodeType;


#pragma mark Creation and Deletion

//Creates an empty node
SFNodeRef SFNodeCreate();

//Parses `sourceString` into a new tree
//`sourceString` is assumed to be encoded with an encoding equivalent to ASCII up to 7-bits. eg ASCII, UTF-8, Latin-1, etc.
SFNodeRef SFNodeCreateFromString(const char* sourceString);

SFNodeRef SFNodeCreateFromFile(FILE *file);

//Perform a deep copy
SFNodeRef SFNodeCopy(SFNodeRef nodeRef);

void SFNodeFreeNonRecursive(SFNodeRef nodeRef);

//Frees a node and all subnodes, incuding strings
void SFNodeFree(SFNodeRef node);


#pragma mark Properties

SFNodeType SFNodeGetType(SFNodeRef node);
void SFNodeSetType(SFNodeRef node, SFNodeType newType);

SFNodeRef SFNodeParent(SFNodeRef node);
void SFNodeSetParent(SFNodeRef node, SFNodeRef parent);

SFNodeRef SFNodeFirstChild(SFNodeRef parent);
void SFNodeSetFirstChild(SFNodeRef parent, SFNodeRef child);

SFNodeRef SFNodeNextInList(SFNodeRef node);
void SFNodeSetNextInList(SFNodeRef node, SFNodeRef nextNode); //Beware! This function simply sets the next pointer, it doesn't attempt to do any splicing

const char* SFNodeHead(SFNodeRef node);
void SFNodeSetHead(SFNodeRef node, const char *head);

size_t SFNodeStringValueLength(SFNodeRef node);
const char* SFNodeStringValue(SFNodeRef node);
void SFNodeSetStringValue(SFNodeRef node, const char *str);

//Copies the string representation of `node` into `stringDestination`.
//selfml_node_copy_string() does NOT append a NULL character.
//`stringDestination` must be large enough to hold `selfml_node_string_length(node)` characters.
_Bool SFNodeCopyStringValueTo(SFNodeRef node, const char* stringDestination);

void SFNodeAddString(SFNodeRef parent, const char* str);
void SFNodeAddChild(SFNodeRef parent, SFNodeRef node);


#pragma mark Representations

void SFNodeWriteRepresentationToFile(SFNodeRef node, FILE* file);
void SFNodePrintRepresentation(SFNodeRef node);
