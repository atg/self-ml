#import "SFONode.h"

@interface SFONode ()

- (void)cleanUp;

@end


@implementation SFONode

#pragma mark Creation

+ (id)node
{
	return [[[self alloc] init] autorelease];
}
+ (id)nodeFromString:(NSString *)string
{
	return [[[self alloc] initWithString:string] autorelease];
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
	//IMPLEMENT ME
}


#pragma mark Properties

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


@synthesize children;

//IMPLEMENT OTHER PROPERTIES




- (SFNodeType)sfNodeType
{
	return SFNodeTypeList;
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