#import "SFONode.h"

//External users of this library must uncomment this line
//#define NO_CHFILECACHE

#ifndef NO_CHFILECACHE
#import "CHFileCache.h"
#endif

#ifndef SFO_UTF8_TO_NSSTRING
#define SFO_UTF8_TO_NSSTRING(utf) [[[NSString alloc] initWithUTF8String:utf] autorelease]
#endif

@interface SFONode ()

- (void)cleanUp;

@end


@implementation SFONode

@synthesize children;
@synthesize parent;
@synthesize rootNode;

- (NSArray *)children
{
	if (isSuspended) [self thaw];
	
	return children;
}
- (SFONode *)parent
{
	if (isSuspended) [self thaw];
	
	return parent;
}
- (SFONode *)rootNode
{
	if (isSuspended) [self thaw];
	
	return rootNode;
}

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
+ (id)nodeWithContentsOfFile:(NSString *)path
{
	return [[[self alloc] initWithContentsOfFile:path] autorelease];
}
+ (id)nodeFromNodeRef:(SFNodeRef)ref
{
	return [[[self alloc] initWithNodeRef:ref isLazy:NO] autorelease];
}
+ (id)lazyNodeFromNodeRef:(SFNodeRef)ref
{
	return [[[self alloc] initWithNodeRef:ref isLazy:YES] autorelease];
}

- (id)init
{
	SFNodeRef ref = SFNodeCreate();
	return [self initWithNodeRef:ref];
}
- (id)initWithString:(NSString *)string
{
	if (!string)
		return nil;
	
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
			id s = [strings objectAtIndex:i];
			if (!s)
				return nil;
			
			if (i == 0)
				self.head = s;
			else
				[self addChild:s];
		}
	}
	return self;
}


- (id)initWithData:(NSData *)data
{
	SFNodeRef ref = SFNodeCreateFromString([[[[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding] autorelease] UTF8String]);
	return [self initWithNodeRef:ref];
}
- (id)initWithContentsOfFile:(NSString *)path
{
	self = [super init];
	filePath = path;
	isSuspended = YES;
	return self;
}
- (BOOL)loadFromFilePath
{
	if (!filePath)
		return YES;
	NSString *path = filePath;
	filePath = nil;
	
	shouldCallSuperInit = NO;
	
	
	NSError *err = nil;
	
	/*
	NSTimeInterval t1 = [NSDate timeIntervalSinceReferenceDate];
	NSTimeInterval t2 = [NSDate timeIntervalSinceReferenceDate];
	static NSTimeInterval totalA = 0.0;
	static NSUInteger totalA_count = 0;
	totalA += t2 - t1;
	totalA_count++;
	*/
	
	NSString *str = nil;
	#ifndef NO_CHFILECACHE
	str = [[CHFileCache sharedFileCache] stringForFilePath:path];
	#else
	str = [[NSString alloc] initWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];	
	#endif
	if (!str)
		return NO;
	
	
	//t1 = [NSDate timeIntervalSinceReferenceDate];
	[self initWithString:str];
	/*
	t2 = [NSDate timeIntervalSinceReferenceDate];
	static NSTimeInterval totalB = 0.0;
	static NSUInteger totalB_count = 0;
	totalB += t2 - t1;
	totalB_count++;
	
	NSLog(@"A = %lf [%lf %ul] | B = %lf [%lf %ul]", totalA / totalA_count, totalA, totalA_count, totalB / totalB_count, totalB, totalB_count);
	*/
	
	shouldCallSuperInit = YES;
	return YES;
}

- (id)initWithNodeRef:(SFNodeRef)ref
{
	return [self initWithNodeRef:ref isLazy:NO];
}

//Designated Initializer
- (id)initWithNodeRef:(SFNodeRef)ref isLazy:(BOOL)isLazy
{
	if (shouldCallSuperInit)
		self = [super init];
	
	if (self)
	{
		if (SFNodeGetType(ref) == SFNodeTypeString) {
			if (!SFNodeStringValue(ref))
				return (id)@"";
			return (id)@(SFNodeStringValue(ref));
		}
		
		node = ref;
		
		rootNode = self;
		
		//Check that the backing node isn't null
		if (node == SFNullNode)
			return nil;
		
		isSuspended = YES;
		if (!isLazy)
			[self thaw];
	}
	
	return self;
}
- (void)thaw
{
	if (filePath) [self loadFromFilePath];
	if (!isSuspended)
		return;
		
	isSuspended = NO;
	
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
		id newNode = [[self class] lazyNodeFromNodeRef:currentChild];
		[children addObject:newNode];
		currentChild = SFNodeNextInList(currentChild);
	}
}


- (id)copyWithZone:(NSZone *)zone
{
	if (filePath) [self loadFromFilePath];
	
	return [[[[self class] allocWithZone:zone] initWithNodeRef:SFNodeCopy(node)] autorelease];	
}


#pragma mark Equality, etc

- (BOOL)isEqual:(id<SFONodeChild>)otherNode
{
	if (filePath) [self loadFromFilePath];
	
    if ([otherNode respondsToSelector:@selector(selfmlRepresentation)])
        return [[self selfmlRepresentation] isEqual:[otherNode selfmlRepresentation]];
    return NO;
}


#pragma mark Properties and Getters

- (SFNodeRef)nodeRef
{
	if (filePath) [self loadFromFilePath];
	
	return node;
}

- (NSString *)head
{
	if (filePath) [self loadFromFilePath];
	
	const char* head = SFNodeHead(node);
	if (!head)
		return @"";
	
	return [[[NSString alloc] initWithUTF8String:head] autorelease] ?: @"";
}
- (void)setHead:(NSString *)headString
{
	if (filePath) [self loadFromFilePath];
	
	size_t len = strlen([headString UTF8String]);
	char* head = malloc((len + 1) * sizeof(char));
	strlcpy(head, [headString UTF8String], (len + 1));
	
	if (!head || strlen(head) == 0)
		return;
	
	SFNodeSetHead(node, head);
}

- (NSUInteger)childCount
{
	if (isSuspended) [self thaw];
	
	return [children count];
}

- (id<SFONodeChild>)childAtIndex:(NSUInteger)index
{
	if (isSuspended) [self thaw];
	
	return [children objectAtIndex:index];
}

- (NSUInteger)indexOfChildNode:(id<SFONodeChild>)childNode
{
	if (isSuspended) [self thaw];
	
	return [[self children] indexOfObject:childNode];
}

- (void)replaceChildNodeAtIndex:(NSInteger)index with:(id<SFONodeChild>)newChild
{
	if (isSuspended) [self thaw];
	
	if (index < 0)
		return;
	
	NSUInteger previousCount = [self childCount];
	
	//Add a child node to the end
	[self addChild:newChild];
	
	if (index >= previousCount)
		return;
	
	SFONode *b = [children objectAtIndex:index];
	if ([b respondsToSelector:@selector(setNodeRef:)])
		[b setNodeRef:SFNullNode];
	
	SFNodeReplaceChildAtIndexWithLast(node, index);
    
	if ([self childCount] <= 1)
	{
		return;
	}
	
	if (newChild != nil)
	{
		[children replaceObjectAtIndex:index withObject:[children lastObject]];
		[children removeLastObject];
	}
}

//TODO: IMPLEMENT OTHER PROPERTIES


#pragma mark Tree Manipulation

- (void)addChild:(id<SFONodeChild>)newNode
{	
	if (isSuspended) [self thaw];
	
	id item = newNode;
	
	if ([item sfNodeType] == SFNodeTypeString)
	{
		item = [[item copy] autorelease];
		
		//Get the UTF8 value of item, then create a new child node and append it to node
		size_t len = strlen([(NSString *)item UTF8String]);
		char* str = malloc((len + 1) * sizeof(char));
		strlcpy(str, [(NSString *)item UTF8String], (len + 1));
		
		SFNodeAddString(node, str);
		
		[children addObject:item];
	}
	else if ([item sfNodeType] == SFNodeTypeList)
	{
		//Add item as a child
		SFNodeAddChild(node, [item nodeRef]);
		
		//Remember to set the parent and root node!
		[(SFONode *)item setParent:self];
		[(SFONode *)item setRootNode:[self rootNode]];
		
		[children addObject:item];
	}
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
	if (isSuspended) [self thaw];
	
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
	if (isSuspended) [self thaw];
	
	NSMutableArray *result = [[[NSMutableArray alloc] init] autorelease];
	for(SFONode *child in children) {
		if([child sfNodeType] == SFNodeTypeString) {
			[result addObject:child];
		}
	}
	
	return result;
}

//Extract singleton nodes (like) (this)
- (NSArray *)extractSingletonNodes
{
	if (isSuspended) [self thaw];
	
	NSMutableArray *result = [[[NSMutableArray alloc] init] autorelease];
	for(SFONode *child in children) {
		if([child sfNodeType] == SFNodeTypeList && [[child head] length] > 0 && [child childCount] == 0) {
			[result addObject:child];
		}
	}
	
	return result;
}

- (NSArray *)extractLists
{
	if (isSuspended) [self thaw];
	
	NSMutableArray *result = [[[NSMutableArray alloc] init] autorelease];
	for(SFONode *child in children) {
		if([child sfNodeType] == SFNodeTypeList) {
			[result addObject:child];
		}
	}
	
	return result;
}

- (id)firstIfString
{
	if (isSuspended) [self thaw];
	
	id first = [self first];
	if ([first sfNodeType] == SFNodeTypeString)
		return first;
	return nil;
}
- (id)first
{
	if (isSuspended) [self thaw];
	
	return [[self children] firstObject];
}
- (NSArray *)rest
{
	if (isSuspended) [self thaw];
	
	if ([children count] >= 2)
		return [children subarrayWithRange:NSMakeRange(1, [children count] - 1)];
	return nil;
}
- (id)nodeForKey:(NSString *)key
{
	if (isSuspended) [self thaw];
	
	__strong const char *keyUTF8 = [key UTF8String];
	
	for(SFONode *child in children) {
		if ([child sfNodeType] == SFNodeTypeList && strcmp(SFNodeHead([child nodeRef]), keyUTF8) == 0) {
			return child;
		}
	}
	return nil;
}
- (id)valueForKey:(NSString *)key
{
	if (isSuspended) [self thaw];
	
	SFONode *forKey = [self nodeForKey:key];
	
	if (!forKey)
		return [super valueForKey:key];
	
	NSString *firstIfString = [forKey firstIfString];
	if ([forKey childCount] == 1 && firstIfString)
		return firstIfString;
	
	return forKey;
}
- (void)setValue:(id)value forKey:(NSString *)key
{
	if (isSuspended) [self thaw];
	
	SFONode *forKey = [self nodeForKey:key];
	
	if (!forKey)
		return [super setValue:value forKey:key];
	
	NSString *firstIfString = [forKey firstIfString];
	if ([forKey childCount] == 1 && firstIfString)
	{
		[forKey replaceChildNodeAtIndex:0 with:value];
	}
	else
	{
		NSInteger index = [self indexOfChildNode:forKey];
		if (index < 0 || index >= NSNotFound)
			return;
		
		[self replaceChildNodeAtIndex:index with:value];
	}
}

- (id)valueForUndefinedKey:(NSString *)key
{
	return nil;
}

- (BOOL)hasSingletonNodeWithHead:(NSString *)shead
{
	if (isSuspended) [self thaw];
	
	for (SFONode *child in children)
	{
		if ([child sfNodeType] == SFNodeTypeList && [child childCount] == 0)
		{
			if ([[child head] isEqual:shead])
				return YES;
		}
	}
	
	return NO;
}


#pragma mark Output

//Use NSFileHandle -> fileDescriptor -> fdopen to create a FILE* to feed to SFNodeWriteRepresentationToFile()
- (NSString *)selfmlRepresentation
{
	if (filePath) [self loadFromFilePath];
	
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
		if (isSuspended)
		{
			[self thaw];
			//NSLog(@"isSus: %d", isSuspended);
			//SFNodeFree(node);
		}
		//else
		{
			SFNodeFreeNonRecursive(node);
		}
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
        [mstr appendFormat:@"(%@", SFO_UTF8_TO_NSSTRING(SFNodeHead(node))];
	
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
		_Bool isScalar = SFNodeGetType(r) == SFNodeTypeString;
		_Bool isSingleton = SFNodeGetType(r) == SFNodeTypeList && SFNodeFirstChild(r) == SFNullNode;
		
		if (isRoot)
        {
            if (!isFirstChild)
                [mstr appendFormat:@"\n\n"];
            
            SFONodeWriteRepresentationInner(r, 0, mstr);
        }
		else if (isFirstChild && (isScalar || isSingleton))
		{
			[mstr appendFormat:@" "];
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
	
	if (strlen(strval) == 0)
	{
		[mstr appendString:@"[]"];
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
        [mstr appendString:SFO_UTF8_TO_NSSTRING(strval)];
    }
    else if (isBracketedString && bracketedStringNestingLevel == 0)
    {
        [mstr appendFormat:@"[%@]", SFO_UTF8_TO_NSSTRING(strval)];
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