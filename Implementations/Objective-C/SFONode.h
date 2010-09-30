@interface SFONode : NSObject<NSCopying, SFONodeChild>
{
	SFNodeRef node;
	NSMutableArray *children;
}

#pragma mark Creation

+ (id)node;
+ (id)nodeFromNodeRef:(SFNodeRef)ref;

- (id)init;
- (id)initWithNodeRef:(SFNodeRef)ref;


#pragma mark Properties

@property (readwrite, copy) NSString *head;
@property (readonly) SFONode *parent;

@property (readonly) SFONode *rootNode;

@property (readonly) NSUInteger childCount;
@property (readwrite) NSArray *children;

- (SFONode *)childAtIndex:(NSUInteger)index;
- (NSUInteger)indexOfChildNode:(SFONode *)node;


#pragma mark Tree Manipulation

//If `node` is a SFONode, then `node` is added as a child (`node` may be copied if it already has a parent)
//Otherwise, `[node description]` is added as a child string node
- (void)addChild:(id)node;
- (void)insertChild:(SFONode *)node atIndex:(NSUInteger)index;
- (void)replaceChildAtIndex:(NSUInteger)index withNode:(SFONode *)node;
- (void)removeChildAtIndex:(NSUInteger)index;


#pragma mark Querying

- (NSString *)extract:(NSString *)name;
- (NSDictionary *)extractAll;


#pragma mark Output

- (NSString *)selfmlRepresentation;
- (NSString *)xmlRepresentation;

@end
