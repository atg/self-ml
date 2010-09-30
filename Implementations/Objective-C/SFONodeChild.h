//You can be assured that any child nodes conform to this protocol

@protocol SFONodeChild

- (SFNodeType)sfNodeType;

@end



@implementation NSString (SFONodeChildExtension)

- (SFNodeType)sfNodeType;

@end