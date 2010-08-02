@interface SFONode : NSObject
{
	SFNodeRef node;
	NSMutableArray *children;
}

+ (id)node;
+ (id)nodeFromNodeRef:(SFNodeRef)ref;

@property (readonly) NSString *head;
@property (readonly) SFONode *parent;

@property (readonly) SFONode *rootNode;

@property (readonly) NSUInteger childCount;
@property (readonly) NSArray *children;
- (SFONode *)childAtIndex:(NSUInteger)index;

- (void)addChild:(SFONode *)node;
- (void)insertChild:(SFONode *)node atIndex:(NSUInteger)index;
- (void)replaceChildAtIndex:(NSUInteger)index withNode:(SFONode *)node;
- (NSUInteger)indexOfChildNode:(SFONode *)node;
- (void)removeChildAtIndex:(NSUInteger)index;
- (void)setChildren:(NSArray *)newChildNodes;

@end
