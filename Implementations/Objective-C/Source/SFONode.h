#import <Foundation/Foundation.h>
#import "self-ml.h"
#import "SFONodeChild.h"

@interface SFONode : NSObject<NSCopying, SFONodeChild>
{
	SFNodeRef node;
	NSMutableArray *children;
}

#pragma mark Creation

+ (id)node;
+ (id)nodeFromString:(NSString *)string;
+ (id)nodeFromData:(NSData *)data;
+ (id)nodeFromNodeRef:(SFNodeRef)ref;

- (id)init;
- (id)initWithString:(NSString *)string;
- (id)initWithData:(NSData *)data;
- (id)initWithNodeRef:(SFNodeRef)ref; //Designated Initializer


#pragma mark Properties

@property (readwrite, copy) NSString *head;
@property (readonly) SFONode *parent;

@property (readonly) SFONode *rootNode;

@property (readonly) NSUInteger childCount;
@property (readonly) NSArray *children;

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

//- (NSArray *)extract:(NSString *)name;
//- (NSDictionary *)extractAll;


#pragma mark Output

- (NSString *)selfmlRepresentation;
- (NSString *)xmlRepresentation;

@end
