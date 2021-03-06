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
 *  If you have an questions about the above declaration, please email anything@fileability.net
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

static void SFNodeWriteRepresentationToFileInner(SFNodeRef node, int indentation, FILE* file);
static void SFNodeWriteRepresentationOfListToFile(SFNodeRef node, int indentation, FILE* file);
static void SFNodeWriteRepresentationOfStringToFile(SFNodeRef node, FILE* file);



#pragma mark Creation and Deletion

SFNodeRef SFNodeCreate()
{
	//Allocate a new node
	SFNodeRef node = (SFNodeRef)calloc(1, sizeof(SFNode));
	
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

SFNodeRef SFNodeCreateFromFile(FILE *file)
{
	char *str = NULL;
	
	size_t capacity = 32;
	size_t count = 0;
	str = (char *)malloc(capacity);
	if (!str)
	{
		printf("No memory\n");
		return 0;
	}
	
	while (1)
	{
		if (count + 1 >= capacity)
		{
			capacity *= 2;
			str = realloc(str, capacity);
			if (!str)
			{
				printf("No memory\n");
				return 0;
			}
		}
		
		char c = fgetc(file);
		if (c == '\0' || c == EOF)
		{
			str[count] = '\0';
			break;
		}
		else
		{
			str[count] = c;
		}
		
		count++;
	}
	
	fclose(file);
	
	SFNodeRef node = SFNodeCreateFromString(str);
	
	free(str);
	
	return node;
}

SFNodeRef SFNodeCopy(SFNodeRef nodeRef)
{	
	if (nodeRef == SFNullNode)
		return SFNullNode;
	
	SFNodeRef newNode = SFNodeCreate();
	if (newNode == SFNullNode)
		return SFNullNode;
	
	SFNodeSetType(newNode, SFNodeGetType(nodeRef));
	
	if (SFNodeStringValue(nodeRef))
	{
		const char *strval = SFNodeStringValue(nodeRef);
		size_t strvalLength = strlen(strval);
		char *newStrval = malloc((strvalLength + 1) * sizeof(char));
		strlcpy(newStrval, strval, strvalLength + 1);
		SFNodeSetStringValue(newNode, newStrval);
	}
	
	if (SFNodeHead(nodeRef))
	{
		const char *strval = SFNodeHead(nodeRef);
		size_t strvalLength = strlen(strval);
		char *newStrval = malloc((strvalLength + 1) * sizeof(char));
		strlcpy(newStrval, strval, strvalLength + 1);
		SFNodeSetHead(newNode, newStrval);
	}
	
	SFNodeSetNextInList(newNode, SFNodeCopy(SFNodeNextInList(nodeRef)));
	SFNodeSetFirstChild(newNode, SFNodeCopy(SFNodeFirstChild(nodeRef)));
	
	return newNode;
}

void SFNodeFreeNonRecursive(SFNodeRef nodeRef)
{
	if (nodeRef == SFNullNode)
		return;
	
	if (SFNodeGetType(nodeRef) == SFNodeTypeList)
	{
		if (SFNodeHead(nodeRef))
		{
			free((void *)SFNodeHead(nodeRef));
			SFNodeSetHead(nodeRef, NULL);
		}
	}
	else if (SFNodeGetType(nodeRef) == SFNodeTypeString)
	{
		if (SFNodeStringValue(nodeRef))
		{
			free((void *)SFNodeStringValue(nodeRef));
			SFNodeSetStringValue(nodeRef, NULL);
		}
	}
	
	free(nodeRef);
}

//Recursively free the subtree of `nodeRef`
void SFNodeFree(SFNodeRef nodeRef)
{
	if (nodeRef == SFNullNode)
		return;
	
	if (SFNodeGetType(nodeRef) == SFNodeTypeList)
	{
		if (SFNodeHead(nodeRef))
		{
			free((void *)SFNodeHead(nodeRef));
			SFNodeSetHead(nodeRef, NULL);
		}
				
		SFNodeFree(SFNodeNextInList(nodeRef));
		SFNodeSetNextInList(nodeRef, SFNullNode);
		
		SFNodeFree(SFNodeFirstChild(nodeRef));
		SFNodeSetFirstChild(nodeRef, SFNullNode);
	}
	else if (SFNodeGetType(nodeRef) == SFNodeTypeString)
	{
		if (SFNodeStringValue(nodeRef))
		{
			free((void *)SFNodeStringValue(nodeRef));
			SFNodeSetStringValue(nodeRef, NULL);
		}
	}
	
	free(nodeRef);
}



#pragma mark Parsing

// root := nodes
void parseRoot(const char* sourceString, SFNodeRef rootNode)
{
	//`offset` is a cursor for sourceString while we're parsing
	//Functions take a pointer to it, then increment or decrement it
	//It can be a little tricky to figure out what's happening, but there's a general rule of thumb:
	//  Functions *take* `offset` on the character they want to parse (so a line comment takes `offset` pointing to the '#')
	//  Functions *leave* `offset` on the last character they parsed (so a line comment leaves `offset` pointing to the 'LF')
	size_t offset = 0;
	
	//Parse a list of nodes
	parseNodes(sourceString, strlen(sourceString), rootNode, &offset);
}

// nodes := <node>*
void parseNodes(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{
	// Loop continuously unless we run out of characters to parse
	// We will exit early
	for(; *offset < length; (*offset)++)
	{
		// Check `c` to see what kind of thing we'll be parsing
		char c = sourceString[*offset];
		
		// whitespace is ignoreed
		// ] and } are ERRORS, but we current have no way to report errors so we just ignore them
		if (isspace(c) || c == ']' || c == '}')
		{
			continue;
		}
		// an opening { is an ERROR unless it is immediately followed by a #, in which case it's a block comment
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
				// consume and ignore
				parseCurlyBracketBlock(sourceString, length, node, offset);
			}
		}
		// # denotes an line comment
		else if (c == '#')
		{
			parseLineComment(sourceString, length, node, offset);
		}
		// [ denotes a string literal
		else if (c == '[')
		{
			const char *str = parseBracketString(sourceString, length, node, offset);
			SFNodeAddString(node, str);
		}
		// ` denotes a string literal
		else if (c == '`')
		{
			const char *str = parseBacktickString(sourceString, length, node, offset);
			SFNodeAddString(node, str);
		}
		// ( denotes a subnode
		else if (c == '(')
		{
			// parse a new subnode until it returns control to us
			parseList(sourceString, length, node, offset);
		}
		// ) denotes the end of this node
		else if (c == ')')
		{
			// return control to the parent
			return;
		}
		// any other characters start a verbatim string
		else
		{
			const char *str = parseVerbatimString(sourceString, length, node, offset);
			SFNodeAddString(node, str);
		}
	}
	
	//The rule is to leave on the last character we parsed
	(*offset)--;
}

// head := BACKTICK_STRING | BRACKETED_STRING | VERBATIM_STRING
const char* parseHead(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{
	const char *str = NULL;
	for(; *offset < length; (*offset)++)
	{
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
			str = parseBracketString(sourceString, length, node, offset);
			
			(*offset)++;
			return str;
		}
		else if (c == '`')
		{
			str = parseBacktickString(sourceString, length, node, offset);
			
			(*offset)++;
			return str;
		}
		else
		{
			str = parseVerbatimString(sourceString, length, node, offset);
			
			(*offset)++;
			return str;
		}
	}
	
	return NULL;
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
			
	//Pass 1: Find out the length of the string
	size_t i = 0;
	size_t literalLength = 0;
	for(; *offset + i < length; i++) {
		char c = sourceString[*offset + i];
		
		if (c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}' || isspace(c))
			break;
		
		literalLength++;
	}
	
	//Allocate `literalLength` of memory
	char *buffer = (char *)calloc(1, literalLength + 1);
	
	//Pass 2: Set the characters in `buffer`
	for(i = 0; *offset < length && i < literalLength; (*offset)++, i++) {
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
	
	int nestingLevel = 1;
		
	//Pass 1: Find out the length of the string
	size_t i = 1;
	size_t literalLength = 0;
	for(; *offset + i < length; i++) {
		char c = sourceString[*offset + i];
		
		if (c == '[')
			nestingLevel++;
		else if (c == ']')
		{
			nestingLevel--;
			
			if (nestingLevel == 0)
				break;
		}
				
		literalLength++;
	}
	
	//Allocate `literalLength` of memory
	char *buffer = (char *)calloc(1, literalLength + 1);
	
	//Pass 2: Set the characters in `buffer`
	for((*offset)++, i = 0; *offset < length && i < literalLength; (*offset)++, i++) {
		buffer[i] = sourceString[*offset];
	}
	
	//(*offset)--;
	
	return buffer;
}

// backtickString := '`' ('``' | any)+ '`'
const char* parseBacktickString(const char* sourceString, size_t length, SFNodeRef node, size_t *offset)
{
	if (*offset >= length)
		return NULL;
			
	//Pass 1: Find out the length of the string
	size_t i = 1;
	size_t literalLength = 0;
	for(; *offset + i < length; i++) {
		char c = sourceString[*offset + i];
		
		char next_c = '\0';
		if (*offset + i + 1 < length)
			next_c = sourceString[*offset + i + 1];
		
		if (c == '`' && next_c != '`')
		{
			break;
		}
		
		literalLength++;
		
		if (c == '`' && next_c == '`')
		{
			i++;
		}
	}
	
	//Allocate `literalLength` of memory
	char *buffer = (char *)calloc(1, literalLength + 1);
	
	//Pass 2: Set the characters in `buffer`
	for((*offset)++, i = 0; *offset < length && i < literalLength; (*offset)++, i++) {
		char c = sourceString[*offset];

		char next_c = '\0';
		if (*offset + 1 < length)
			next_c = sourceString[*offset + 1];
		
		if (c == '`' && next_c == '`')
		{
			(*offset)++;
		}
		
		buffer[i] = c;
	}
		
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
	
	//Head
	const char* head = parseHead(sourceString, length, parentNode, offset);
	
	//If no head can be found, do a parse anyway to sort up any loose ends, but don't use the result
	if (head == NULL || strlen(head) == 0)
	{
		parseNodes(sourceString, length, node, offset);
		return;
	}
	
	SFNodeSetHead(node, head);
	
	//Add to its parent
	SFNodeSetParent(node, parentNode);
	SFNodeAddChild(parentNode, node);
	
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
	if (node == SFNullNode)
		return SFNodeTypeList;
	
	return SFNodeForRef(node)->nodeType;
}
void SFNodeSetType(SFNodeRef node, SFNodeType newType)
{
	if (node == SFNullNode)
		return;
	
	SFNodeForRef(node)->nodeType = newType;
}

SFNodeRef SFNodeParent(SFNodeRef node)
{
	if (node == SFNullNode)
		return SFNullNode;
	
	return SFNodeForRef(node)->parent;
}
void SFNodeSetParent(SFNodeRef node, SFNodeRef parent)
{
	if (node == SFNullNode)
		return;
	
	SFNodeForRef(node)->parent = parent;
}

SFNodeRef SFNodeFirstChild(SFNodeRef parent)
{
	if (parent == SFNullNode)
		return SFNullNode;
	
	return SFNodeForRef(parent)->firstChild;
}
void SFNodeSetFirstChild(SFNodeRef parent, SFNodeRef child)
{
	if (parent == SFNullNode)
		return;
	
	SFNodeForRef(parent)->firstChild = child;
}

SFNodeRef SFNodeNextInList(SFNodeRef node)
{
	if (node == SFNullNode)
		return SFNullNode;
	
	return SFNodeForRef(node)->next;
}
//Beware! This function simply sets the next pointer, it doesn't attempt to do any splicing
void SFNodeSetNextInList(SFNodeRef node, SFNodeRef nextNode)
{
	if (node == SFNullNode)
		return;
		
	SFNodeForRef(node)->next = nextNode;
}

const char* SFNodeHead(SFNodeRef node)
{
	if (node && SFNodeGetType(node) == SFNodeTypeList)
		return SFNodeForRef(node)->stringValue;
	return NULL;
}
void SFNodeSetHead(SFNodeRef node, const char *head)
{
	if (node == SFNullNode)
		return;
	
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
	if (parent == SFNullNode || node == SFNullNode)
		return;
	
	//If the parent isn't a list, then it can't have any children. Ignore the request to add a child.
	if (SFNodeGetType(parent) != SFNodeTypeList)
		return;
	
	//No, a node cannot be its own parent. Such matters are frowned upon
	if (parent == node)
		return;
	
	SFNodeRef firstChild = SFNodeFirstChild(parent);
	
	//If the parent has no first child, then set it to the new node
	if (firstChild == SFNullNode)
	{
		SFNodeSetFirstChild(parent, node);
		return;
	}
	
	//Otherwise find its last child
	SFNodeRef currentNode = firstChild;
	while (currentNode != SFNullNode)
	{
		if (currentNode == node)
			break;
		
		if (SFNodeNextInList(currentNode) == SFNullNode)
		{
			SFNodeSetNextInList(currentNode, node);
            break;
        }
        
        currentNode = SFNodeNextInList(currentNode);
	}
}

unsigned SFChildNodeCount(SFNodeRef node)
{
	SFNodeRef firstChild = SFNodeFirstChild(node);
	if (firstChild == SFNullNode)
		return 0;
	
	SFNodeRef nextChild = firstChild;
	unsigned count = 1;
	while (1)
	{
		nextChild = SFNodeNextInList(nextChild);
		if (nextChild == SFNullNode)
			return count;
		
		count++;
	}
}

SFNodeRef SFChildNodeAtIndex(SFNodeRef parent, unsigned index)
{
	SFNodeRef firstChild = SFNodeFirstChild(parent);
	if (firstChild == SFNullNode || index == 0)
    {
        return firstChild;
    }
	
	SFNodeRef nextChild = firstChild;
	unsigned i = 1;
	while (1)
	{
		nextChild = SFNodeNextInList(nextChild);
		if (i == index || nextChild == SFNullNode)
        {
            return nextChild;
        }
		
		i++;
	}
}

void SFNodeReplaceChildAtIndexWithLast(SFNodeRef parent, unsigned index)
{
	unsigned count = SFChildNodeCount(parent);
	
	//pre_a is the node before a, or NULL if it doesn't exist
	SFNodeRef pre_a = index >= 1 ? SFChildNodeAtIndex(parent, index - 1) : SFNullNode;
	
	//a is the node we're replacing
	SFNodeRef a = SFChildNodeAtIndex(parent, index);
	
	//pre_z is the node before z, or NULL if it doesn't exist
	SFNodeRef pre_z = count >= 2 ? SFChildNodeAtIndex(parent, count - 2) : SFNullNode;
	
	//z is the node we're moving
	SFNodeRef z = count >= 1 ? SFChildNodeAtIndex(parent, count - 1) : SFNullNode;
	
	//printf("we have | count = %d | index = %d | a = %d | z = %d | pre_a = %d | pre_z = %d\n", count, index, a, z, pre_a, pre_z);
	//SFNodePrintRepresentation(parent);
	if (count == 0 || count == 1 || index + 1 >= count || z == SFNullNode || a == SFNullNode)
	{
		//printf("bailing out\n", count, index, a, z);
		return;
	}
	
	if (pre_a == SFNullNode)
	{
		// parent ->> a -> ...
		
		if (pre_z == parent || pre_z == pre_a || pre_z == a || pre_z == z || pre_z == SFNullNode)
		{
			// We have
			//   parent ->> a -> z
			// We want
			//   parent ->> z
			
			// link parent ->> z
			SFNodeSetFirstChild(parent, z);
			
			// unlink a -> z
			SFNodeSetNextInList(a, SFNullNode);
			
			// free a
			SFNodeFree(a);
		}
		else
		{
			// We have
			//   parent ->> a -> ... -> pre_z -> z
			// We want
			//   parent ->> z -> ... -> pre_z
			
			// get a -> post_a
			SFNodeRef post_a = SFNodeNextInList(a);
			
			// unlink a -> post_a
			SFNodeSetNextInList(a, SFNullNode);
			
			// free a
			SFNodeFree(a);
			
			// link parent ->> z
			SFNodeSetFirstChild(parent, z);
			
			// link z -> post_a
			SFNodeSetNextInList(z, post_a);			
			
			// unlink pre_z -> z
			SFNodeSetNextInList(pre_z, SFNullNode);
		}
	}
	else
	{
		// parent ->> pre_a -> a -> ...
		
		if (pre_z == parent || pre_z == pre_a || pre_z == a || pre_z == z || pre_z == SFNullNode)
		{
			// We have
			//   parent ->> pre_a -> a -> z
			// We want
			//   parent ->> pre_a -> z
			
			// link pre_a -> z
			SFNodeSetNextInList(pre_a, z);
			
			// unlink a
			SFNodeSetNextInList(a, SFNullNode);
			
			// free a
			SFNodeFree(a);
		}
		else
		{
			// We have
			//   parent ->> pre_a -> a -> ... -> pre_z -> z
			// We want
			//   parent ->> pre_a -> z -> ... -> pre_z
			
			// get a -> post_a
			SFNodeRef post_a = SFNodeNextInList(a);
			
			// unlink a -> post_a
			SFNodeSetNextInList(a, SFNullNode);
			
			// free a
			SFNodeFree(a);
			
			// link pre_a -> z
			SFNodeSetNextInList(pre_a, z);
			
			// link z -> post_a
			SFNodeSetNextInList(z, post_a);			
			
			// unlink pre_z -> z
			SFNodeSetNextInList(pre_z, SFNullNode);
		}
	}
	//SFNodePrintRepresentation(parent);
	//printf("end\n");
}

void SFNodeReplaceChildAtIndexWithLast_old(SFNodeRef parent, unsigned index)
{
	// If we have
	// ... -> a -> b -> c -> ... -> y -> z
	// Then we want to unlink y -> z, unlink b -> c and relink a -> z
	
	unsigned count = SFChildNodeCount(parent);
	
	SFNodeRef a = index >= 1 ? SFChildNodeAtIndex(parent, index - 1) : SFNullNode;
	SFNodeRef b = SFChildNodeAtIndex(parent, index);
	SFNodeRef y = count >= 2 ? SFChildNodeAtIndex(parent, count - 2) : SFNullNode;
	SFNodeRef z = count >= 1 ? SFChildNodeAtIndex(parent, count - 1) : SFNullNode;
	
	SFNodePrintRepresentation(parent);
	
	if (z == SFNullNode)
	{
		printf("\t is null - ending\n");
		return;
	}
	
	if (count == 0 || count == 1)
	{
		printf("\t count is %d - ending\n", count);
		return;
	}

	printf("\t a %d -- b %d -- y %d -- z %d -- parent %d -- index %d \n", a, b, y, z, parent, index);
	if (count == 2)
	{
        printf("count == 2\n");
		if (b != SFNullNode && z != SFNullNode)
		{
			//This is a little tricky. We need to replace a with z
			
			// relink parent ->> z
			SFNodeSetFirstChild(parent, z);
			
			// unlink a -> z
			SFNodeSetNextInList(b, SFNullNode);
			
			//Free a
			SFNodeFree(b);
		}
		
		return;
	}
	
	if (index == 0 && a == SFNullNode)
	{
		// We have
		// parent ->> b -> ... -> y -> z
		return;
	}
	
	// Unlink y -> z
	SFNodeSetNextInList(y, SFNullNode);
	
	// Unlink b -> c
	SFNodeSetNextInList(b, SFNullNode);
	
	//Relink a -> z
	SFNodeSetNextInList(a, z);
	
	//Free b
	SFNodeFree(b);
	SFNodePrintRepresentation(parent);
	printf("\t end\n");
}


size_t SFNodeStringValueLength(SFNodeRef node)
{
	if (node == SFNullNode)
		return 0;
	
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
	if (node && SFNodeGetType(node) == SFNodeTypeString)
		return SFNodeForRef(node)->stringValue;
	return NULL;
}
void SFNodeSetStringValue(SFNodeRef node, const char *str)
{
	if (node == SFNullNode)
		return;
	
	if (SFNodeGetType(node) == SFNodeTypeString)
		SFNodeForRef(node)->stringValue = str;
}

//Copies the string representation of `node` into `stringDestination`.
//SFNodeCopyStringValueTo() does NOT append a NULL character.
//`stringDestination` must be large enough to hold `SFNodeStringValueLength(node)` characters.
_Bool SFNodeCopyStringValueTo(SFNodeRef node, const char* stringDestination)
{
	if (node == SFNullNode)
		return SFNullNode;
	
	if (SFNodeStringValueLength(node) == 0)
		return false;
	
	memcpy((void *)stringDestination, (void *)SFNodeStringValue(node), SFNodeStringValueLength(node));
	return true;
}


#pragma mark Representations

size_t SFNodeRepresentationLength(SFNodeRef node)
{
	//TODO: Needs implementation
	return 0;
}
_Bool SFNodeRepresentation(SFNodeRef node, const char* stringDestination)
{
	//TODO: Needs implementation
	return false;
}


void SFNodePrintRepresentation(SFNodeRef node)
{
	SFNodeWriteRepresentationToFile(node, stdout);
	
	printf("\n");
}
void SFNodeWriteRepresentationToFile(SFNodeRef node, FILE* file)
{
	if (node == SFNullNode)
		return;
	
	SFNodeWriteRepresentationToFileInner(node, 0, file);
}
void SFNodeWriteRepresentationToFileInner(SFNodeRef node, int indentation, FILE* file)
{
	if (node == SFNullNode)
		return;
	
	int i;
	for (i = 0; i < indentation; i++)
	{
		fprintf(file, "    ");
	}
	
    if (SFNodeGetType(node) == SFNodeTypeList)
    {
		SFNodeWriteRepresentationOfListToFile(node, indentation, file);
    }
    else if (SFNodeGetType(node) == SFNodeTypeString)
    {  
		SFNodeWriteRepresentationOfStringToFile(node, file);
    }
}

void SFNodeWriteRepresentationOfListToFile(SFNodeRef node, int indentation, FILE* file)
{
	if (node == SFNullNode)
		return;
	
	const char *head = SFNodeHead(node);
	_Bool isRoot = head == NULL;
	
	if (!isRoot)
		fprintf(file, "(%s", SFNodeHead(node));
	
	SFNodeRef r = SFNodeFirstChild(node);
	_Bool isScalarOnly = true;
	if (isRoot)
	{
		isScalarOnly = false;
	}
	else
	{
		while (r != SFNullNode)
		{
			if (SFNodeGetType(r) == SFNodeTypeList)
				isScalarOnly = false;
			
			r = SFNodeNextInList(r);
		}
	}
	
	r = SFNodeFirstChild(node);
	_Bool isFirstChild = true;
	while (r != SFNullNode)
	{
		if (isRoot)
		{
			if (!isFirstChild)
				fprintf(file, "\n\n");
			
			SFNodeWriteRepresentationToFileInner(r, 0, file);
		}
		else if (isScalarOnly)
		{
			fprintf(file, " ");
			SFNodeWriteRepresentationToFileInner(r, 0, file);
		}
		else
		{
			fprintf(file, "\n");
			SFNodeWriteRepresentationToFileInner(r, indentation + 1, file);
		}
		
		r = SFNodeNextInList(r);
		isFirstChild = false;
	}
	
	if (!isRoot)
		fprintf(file, ")");
}
void SFNodeWriteRepresentationOfStringToFile(SFNodeRef node, FILE* file)
{
	if (node == SFNullNode)
		return;
	
	const char *strval = SFNodeStringValue(node);
	if (strval == NULL)
		return;
	
	if (strlen(strval) == 0)
	{
		fprintf(file, "[]");
		return;
	}
	
	//Find out if scannerStrval can be written as a verbatim string or bracketed string
	_Bool isVerbatimString = true;
	_Bool isBracketedString = true;
	
	int bracketedStringNestingLevel = 0;
	const char *scannerStrval = strval;
	for (; *scannerStrval != '\0'; scannerStrval++)
	{
		if (isspace(*scannerStrval))
		{
			isVerbatimString = false;
		}
		
		if (*scannerStrval == '[')
			bracketedStringNestingLevel++;
		else if (*scannerStrval == ']')
			bracketedStringNestingLevel--;
		
		if (bracketedStringNestingLevel == -1)
			isBracketedString = false;
		
		switch (*scannerStrval) {
			case '#':
			case '`':
			case '(':
			case ')':
			case '[':
			case ']':
			case '{':
			case '}':
				isVerbatimString = false;
			default: continue;
		}
	}
	
	
	if (isVerbatimString)
	{
		fprintf(file, "%s", strval);
	}
	else if (isBracketedString && bracketedStringNestingLevel == 0)
	{
		fprintf(file, "[%s]", strval);
	}
	else
	{
		fprintf(file, "`");
		for (; *strval != '\0'; strval++)
		{
			if (*strval == '`')
				fprintf(file, "`");
			
			fprintf(file, "%c", *strval);
		}
		fprintf(file, "`");
	}
}
