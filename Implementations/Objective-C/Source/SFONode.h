#import <Foundation/Foundation.h>
#import "self-ml.h"
#import "SFONodeChild.h"

#ifndef SELFML
#define SELFML(items...) [SFONode nodeFromList:[NSArray arrayWithObjects:items, nil]];
#endif

@interface SFONode : NSObject<NSCopying, SFONodeChild>
{
	SFONode *parent;
	SFNodeRef node;
	NSMutableArray *children;
}

#pragma mark Creation

+ (id)node;
+ (id)nodeFromString:(NSString *)string;
+ (id)nodeFromList:(NSArray *)strings;
+ (id)nodeFromData:(NSData *)data;
+ (id)nodeFromNodeRef:(SFNodeRef)ref;

- (id)init;
- (id)initWithString:(NSString *)string;
- (id)initWithList:(NSArray *)strings; // [head, arg1, arg2, arg3, ...]
- (id)initWithData:(NSData *)data;
- (id)initWithNodeRef:(SFNodeRef)ref; //Designated Initializer


#pragma mark Equality, etc

- (BOOL)isEqual:(id<SFONodeChild>)otherNode;


#pragma mark Properties and Getters

@property (readwrite, copy) NSString *head;
@property (readwrite, assign) SFONode *parent; //readwrite for internal reasons. External classes should not use this setter

@property (readwrite, assign) SFONode *rootNode; //readwrite for internal reasons. DExternal classes should not use this setter

@property (readonly, assign) NSUInteger childCount;
@property (readonly) NSArray *children;
@property (readonly, assign) SFNodeRef nodeRef;

//Returns nil if index is invalid
- (id<SFONodeChild>)childAtIndex:(NSUInteger)index;

//Returns NSNotFound if there is no node at that index
- (NSUInteger)indexOfChildNode:(id<SFONodeChild>)childNode;


#pragma mark Tree Manipulation

//If `node` is a SFONode, then `node` is added as a child (`node` may be copied if it already has a parent)
//Otherwise, `[node description]` is added as a child string node
- (void)addChild:(id<SFONodeChild>)newNode;

/*
- (void)insertChild:(id<SFONodeChild>)childNode atIndex:(NSUInteger)index;
- (void)replaceChildAtIndex:(NSUInteger)index withNode:(id<SFONodeChild>)childNode;
- (void)removeChildAtIndex:(NSUInteger)index;
*/

#pragma mark Querying

//Extract an NSArray of all child nodes with name nodeName
- (NSArray *)extract:(NSString *)nodeName;

//Extract all strings
- (NSArray *)extractStrings;


#pragma mark Output

- (NSString *)selfmlRepresentation;
- (NSString *)xmlRepresentation;

#pragma mark some great functions
void SFONodeWriteRepresentation(SFNodeRef node, NSMutableString *mstr);
void SFONodeWriteRepresentationInner(SFNodeRef node, int indentation, NSMutableString *mstr);
void SFONodeWriteRepresentationOfList(SFNodeRef node, int indentation, NSMutableString *mstr);
void SFONodeWriteRepresentationOfString(SFNodeRef node, NSMutableString *mstr);

@end
