#import <Foundation/Foundation.h>
#import "self-ml.h"
#import "SFONodeChild.h"

#ifndef SELFML
#define SELFML(items...) [SFONode nodeFromList:[NSArray arrayWithObjects:items, nil]]
#endif

@interface SFONode : NSObject<NSCopying, SFONodeChild>
{
    SFONode *rootNode;
	SFONode *parent;
	SFNodeRef node;
	NSMutableArray *children;
	
	BOOL shouldCallSuperInit;
	BOOL isSuspended;
	NSString *filePath;
}

#pragma mark Creation

+ (instancetype)node;
+ (instancetype)nodeFromString:(NSString *)string;
+ (instancetype)nodeFromList:(NSArray *)strings;
+ (instancetype)nodeFromData:(NSData *)data;
+ (instancetype)nodeFromNodeRef:(SFNodeRef)ref;
+ (instancetype)nodeWithContentsOfFile:(NSString *)path;

- (BOOL)loadFromFilePath;

- (id)init;
- (id)initWithString:(NSString *)string;
- (id)initWithList:(NSArray *)strings; // [head, arg1, arg2, arg3, ...]
- (id)initWithData:(NSData *)data;
- (id)initWithNodeRef:(SFNodeRef)ref;
- (id)initWithNodeRef:(SFNodeRef)ref isLazy:(BOOL)isLazy; //Designated Initializer

- (id)initWithContentsOfFile:(NSString *)path;

#pragma mark Equality, etc

- (BOOL)isEqual:(id<SFONodeChild>)otherNode;


#pragma mark Properties and Getters

@property (copy) NSString *head;
@property SFONode *parent; //readwrite for internal reasons. External classes should not use this setter

@property SFONode *rootNode; //readwrite for internal reasons. DExternal classes should not use this setter

@property (readonly) NSUInteger childCount;
@property (readonly) NSArray *children;
@property SFNodeRef nodeRef;

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

- (void)replaceChildNodeAtIndex:(NSInteger)index with:(id<SFONodeChild>)newChild;

#pragma mark Querying

//Extract an NSArray of all child nodes with name nodeName
- (NSArray *)extract:(NSString *)nodeName;

//Extract all strings
- (NSMutableArray *)extractStrings;

//Extract singleton nodes (like) (this)
- (NSArray *)extractSingletonNodes;

//Extract all strings
- (NSArray *)extractLists;

//Return a node with a specific head
- (id)valueForKey:(NSString *)key;

//Query the existence of a singleton node with a specified head
- (BOOL)hasSingletonNodeWithHead:(NSString *)shead;


#pragma mark Other

- (id)nodeForKey:(NSString *)key;
- (id)firstIfString;
- (id)first;
- (NSArray *)rest;

#pragma mark Output

- (NSString *)selfmlRepresentation;
- (NSString *)xmlRepresentation;

#pragma mark some great functions
void SFONodeWriteRepresentation(SFNodeRef node, NSMutableString *mstr);
void SFONodeWriteRepresentationInner(SFNodeRef node, int indentation, NSMutableString *mstr);
void SFONodeWriteRepresentationOfList(SFNodeRef node, int indentation, NSMutableString *mstr);
void SFONodeWriteRepresentationOfString(SFNodeRef node, NSMutableString *mstr);

@end
