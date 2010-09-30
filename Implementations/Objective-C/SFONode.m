@implementation SFONode

#pragma mark Creation

+ (id)node
{
	return [[[self alloc] init] autorelease];
}
+ (id)nodeFromNodeRef:(SFNodeRef)ref
{
	return [[[self alloc] initWithNodeRef:ref] autorelease];
}

+ (id)init
{
	SFNodeRef ref = SFNodeCreate();
	return [self initWithNodeRef:ref];
}
+ (id)initWithNodeRef:(SFNodeRef)ref
{
	return [[[self alloc] initWithNodeRef:ref] autorelease];
}


#pragma mark Properties

- (NSString *)head
{
	const char* head = SFNodeHead(ref);
	if (!head)
		return @"";
		
	return [[[NSString alloc] initWithUTF8String:head] autorelease] ?: @"";
}
- (void)setHead:(NSString *)headString
{
	const char* head = [headString UTF8String];
	if (!head || strlen(head) == 0)
		return;
	
	SFNodeSetHead(ref, head);
}




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
	if (!ref)
		return;
	
	SFNodeFree(ref);
	ref = SFNullNode;
}

@end