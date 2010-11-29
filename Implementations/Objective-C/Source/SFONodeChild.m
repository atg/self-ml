#import "SFONodeChild.h"

@implementation NSString (SFONodeChildExtension)

- (SFNodeType)sfNodeType
{
	return SFNodeTypeString;
}

- (id)firstIfString
{
	return self;
}
- (NSArray *)extractStrings
{
	return [NSArray arrayWithObject:self];
}

@end