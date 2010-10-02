#import "SFONode.h"

@interface SFONode ()

- (void)cleanUp;

@end


@implementation SFONode

@synthesize children;
@synthesize parent;
@synthesize rootNode;

#pragma mark Creation

+ (id)node
{
	return [[[self alloc] init] autorelease];
}
+ (id)nodeFromString:(NSString *)string
{
	return [[[self alloc] initWithString:string] autorelease];
}
+ (id)nodeFromList:(NSArray *)strings
{
	return [[[self alloc] initWithList:strings] autorelease];
}
+ (id)nodeFromData:(NSData *)data
{
	return [[[self alloc] initWithData:data] autorelease];
}
+ (id)nodeFromNodeRef:(SFNodeRef)ref
{
	return [[[self alloc] initWithNodeRef:ref] autorelease];
}

- (id)init
{
	SFNodeRef ref = SFNodeCreate();
	return [self initWithNodeRef:ref];
}
- (id)initWithString:(NSString *)string
{
	SFNodeRef ref = SFNodeCreateFromString([string UTF8String]);
	return [self initWithNodeRef:ref];
}
- (id)initWithList:(NSArray *)strings
{
	if (self = [self init])
	{
		NSUInteger i = 0;
		for (i = 0; i < [strings count]; i++)
		{
			if (i == 0)
				self.head = [strings objectAtIndex:i];
			else
				[self addChild:[strings objectAtIndex:i]];
		}
	}
	return self;
}


- (id)initWithData:(NSData *)data
{
	SFNodeRef ref = SFNodeCreateFromString([[[[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding] autorelease] UTF8String]);
	return [self initWithNodeRef:ref];
}

//Designated Initializer
- (id)initWithNodeRef:(SFNodeRef)ref
{
	if (self = [super init])
	{
		node = ref;
		
		rootNode = self;
		
		//Check that the backing node isn't null
		if (node == SFNullNode)
			return nil;
		
		
		//Count the number of children
		NSUInteger childCount = 0;
		SFNodeRef currentChild = SFNodeFirstChild(node);
		while (currentChild != SFNullNode)
		{
			childCount++;
			currentChild = SFNodeNextInList(currentChild);
		}
		
		
		//Add the children, along with new nodes
		children = [[NSMutableArray alloc] initWithCapacity:childCount];
		currentChild = SFNodeFirstChild(node);
		while (currentChild != SFNullNode)
		{
			[children addObject:[[self class] nodeFromNodeRef:currentChild]];
			currentChild = SFNodeNextInList(currentChild);
		}
	}
	
	return self;
}


- (id)copyWithZone:(NSZone *)zone
{
	return [[[SFONode allocWithZone:zone] initWithNodeRef:SFNodeCopy(node)] autorelease];
	
}


#pragma mark Equality, etc

- (BOOL)isEqual:(id<SFONodeChild>)otherNode
{
	return [[self selfmlRepresentation] isEqual:[otherNode selfmlRepresentation]];
}


#pragma mark Properties and Getters

- (SFNodeRef)nodeRef
{
	return node;
}

- (NSString *)head
{
	const char* head = SFNodeHead(node);
	if (!head)
		return @"";
	
	return [[[NSString alloc] initWithUTF8String:head] autorelease] ?: @"";
}
- (void)setHead:(NSString *)headString
{
	size_t len = strlen([headString UTF8String]);
	char* head = malloc((len + 1) * sizeof(char));
	strlcpy(head, [headString UTF8String], (len + 1));
	
	if (!head || strlen(head) == 0)
		return;
	
	SFNodeSetHead(node, head);
}

- (NSUInteger)childCount
{
	return [children count];
}

- (id<SFONodeChild>)childAtIndex:(NSUInteger)index
{
	return [children objectAtIndex:index];
}

- (NSUInteger)indexOfChildNode:(id<SFONodeChild>)childNode
{
	return [[self children] indexOfObject:childNode];
}

//TODO: IMPLEMENT OTHER PROPERTIES


#pragma mark Tree Manipulation

- (void)addChild:(id<SFONodeChild>)newNode
{
	id copy = [[newNode copy] autorelease];
	
	if ([newNode sfNodeType] == SFNodeTypeString)
	{
		//Get the UTF8 value of newNode, then create a new child node and append it to node
		size_t len = strlen([(NSString *)newNode UTF8String]);
		char* str = malloc((len + 1) * sizeof(char));
		strlcpy(str, [(NSString *)newNode UTF8String], (len + 1));
		
		SFNodeAddString(node, str);
	}
	else if ([newNode sfNodeType] == SFNodeTypeList)
	{
		//Copy newNode and add it as a child
		SFNodeAddChild(node, [copy nodeRef]);
		
		//Remember to set the parent and root node!
		[(SFONode *)newNode setParent:self];
		[(SFONode *)newNode setRootNode:[self rootNode]];
	}
	[children addObject:copy];
}

/*
- (void)insertChild:(id<SFONodeChild>)childNode atIndex:(NSUInteger)index
{
	//TODO: IMPLEMENT ME
}
- (void)replaceChildAtIndex:(NSUInteger)index withNode:(id<SFONodeChild>)childNode
{
	//TODO: IMPLEMENT ME
}
- (void)removeChildAtIndex:(NSUInteger)index
{
	//TODO: IMPLEMENT ME
}
*/


- (SFNodeType)sfNodeType
{
	return SFNodeTypeList;
}


#pragma mark Querying

//Extract an NSArray of all child nodes with name nodeName
- (NSArray *)extract:(NSString *)nodeName
{
	NSMutableArray *result = [[[NSMutableArray alloc] init] autorelease];
	for(SFONode *child in children) {
		if ([child sfNodeType] == SFNodeTypeList && [[child head] isEqual:nodeName]) {
			[result addObject:child];
		}
	}
	return result;
}

//Extract all strings
- (NSArray *)extractStrings
{
	
	NSMutableArray *result = [[[NSMutableArray alloc] init] autorelease];
	for(SFONode *child in children) {
		if([child sfNodeType] == SFNodeTypeString && [[child head] length] > 0) {
			[result addObject:child];
		}
	}
	
	return result;

}


#pragma mark Output

//Use NSFileHandle -> fileDescriptor -> fdopen to create a FILE* to feed to SFNodeWriteRepresentationToFile()
- (NSString *)selfmlRepresentation
{
	NSMutableString *stringRep = [[[NSMutableString alloc] init] autorelease];
	SFONodeWriteRepresentation([self nodeRef], stringRep);
	return stringRep;
	
}

//Use NSXMLDocument to create an XML string
- (NSString *)xmlRepresentation
{
	//TODO: IMPLEMENT ME
	return nil;
}


#pragma mark Cleanup

/*
- (void)dealloc
{
	[self cleanUp];
	
	NSLog(@" -");
	NSLog(@"children = %d", [children count]);
	[children release];
	
	[super dealloc];
}
*/
- (void)finalize
{
	[self cleanUp];
	[super finalize];
}
- (void)cleanUp
{	
	if (node != SFNullNode)
	{
		SFNodeFreeNonRecursive(node);
		node = SFNullNode;
	}
}

#pragma mark Functions
void SFONodeWriteRepresentation(SFNodeRef node, NSMutableString *mstr)
{
    if (node == SFNullNode)
        return;
    
    SFONodeWriteRepresentationInner(node, 0, mstr);
}
void SFONodeWriteRepresentationInner(SFNodeRef node, int indentation, NSMutableString *mstr)
{
    if (node == SFNullNode)
        return;
    
    int i;
    for (i = 0; i < indentation; i++)
    {
        [mstr appendFormat:@"    "];
    }
    
    if (SFNodeGetType(node) == SFNodeTypeList)
    {
        SFONodeWriteRepresentationOfList(node, indentation, mstr);
    }
    else if (SFNodeGetType(node) == SFNodeTypeString)
    {  
        SFONodeWriteRepresentationOfString(node, mstr);
    }
}

void SFONodeWriteRepresentationOfList(SFNodeRef node, int indentation, NSMutableString *mstr)
{
    if (node == SFNullNode)
        return;
	
    const char *head = SFNodeHead(node);
    _Bool isRoot = head == NULL;
	
    if (!isRoot)
        [mstr appendFormat:@"(%s", SFNodeHead(node)];
	
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
                [mstr appendFormat:@"\n\n"];
            
            SFONodeWriteRepresentationInner(r, 0, mstr);
        }
        else if (isScalarOnly)
        {
            [mstr appendFormat:@" "];
            SFONodeWriteRepresentationInner(r, 0, mstr);
        }
        else
        {
            [mstr appendFormat:@"\n"];
            SFONodeWriteRepresentationInner(r, indentation + 1, mstr);
        }
		
        r = SFNodeNextInList(r);
        isFirstChild = false;
    }
	
    if (!isRoot)
        [mstr appendFormat:@")"];
}
void SFONodeWriteRepresentationOfString(SFNodeRef node, NSMutableString *mstr)
{
    if (node == SFNullNode)
        return;
    
    const char *strval = SFNodeStringValue(node);
    if (strval == NULL)
        return;
	
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
        [mstr appendFormat:@"%s", strval];
    }
    else if (isBracketedString && bracketedStringNestingLevel == 0)
    {
        [mstr appendFormat:@"[%s]", strval];
    }
    else
    {
        [mstr appendFormat:@"`"];
        for (; *strval != '\0'; strval++)
        {
            if (*strval == '`')
                [mstr appendFormat:@"`"];
			
            [mstr appendFormat:@"%c", *strval];
        }
        [mstr appendFormat:@"`"];
    }
}


@end