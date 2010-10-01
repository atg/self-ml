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
- (id)initWithList:(NSString *)strings
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
	//TODO: IMPLEMENT ME
}


#pragma mark Equality, etc

- (BOOL)isEqual:(id<SFONodeChild>)otherNode
{
	//TODO: IMPLEMENT ME
}


#pragma mark Properties and Getters

- (NSString *)head
{
	const char* head = SFNodeHead(node);
	if (!head)
		return @"";
	
	return [[[NSString alloc] initWithUTF8String:head] autorelease] ?: @"";
}
- (void)setHead:(NSString *)headString
{
	const char* head = [headString UTF8String];
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
	//TODO: IMPLEMENT ME
}

- (NSUInteger)indexOfChildNode:(id<SFONodeChild>)childNode
{
	//TODO: IMPLEMENT ME
}

//TODO: IMPLEMENT OTHER PROPERTIES


#pragma mark Tree Manipulation

- (void)addChild:(id<SFONodeChild>)newNode
{
	//TODO: IMPLEMENT ME
	
	if ([newNode isKindOfClass:[NSString class]])
	{
		//Get the UTF8 value of newNode, then create a new child node and append it to node
	}
	else if ([newNode isKindOfClass:[SFONode class]])
	{
		//Copy newNode and add it as a child
	}
	
	//Remember to set the parent and root node!
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
	//TODO: IMPLEMENT ME
}

//Extract all strings
- (NSArray *)extractStrings
{
	//TODO: IMPLEMENT ME
}


#pragma mark Output

//Use NSFileHandle -> fileDescriptor -> fdopen to create a FILE* to feed to SFNodeWriteRepresentationToFile()
- (NSString *)selfmlRepresentation
{
	//TODO: IMPLEMENT ME
}

//Use NSXMLDocument to create an XML string
- (NSString *)xmlRepresentation
{
	//TODO: IMPLEMENT ME
}


#pragma mark Cleanup

- (void)dealloc
{
	[self dealloc];
	[super dealloc];
}
- (void)finalize
{
	[self cleanUp];
	[super finalize];
}
- (void)cleanUp
{
	if (node == SFNullNode)
		return;
	
	SFNodeFree(node);
	node = SFNullNode;
}

@end