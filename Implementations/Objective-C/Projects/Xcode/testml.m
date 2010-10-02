#import <Foundation/Foundation.h>
#import <self-ml/SFONode.h>

int main (int argc, const char * argv[]) {
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	
	SFONode *testNode1 = [SFONode node];
	SFONode *testNode2 = [SFONode node];
	[testNode1 setHead:@"foo"];
	[testNode2 setHead:@"bar"];
	
	[testNode1 addChild:testNode2];
	
	
	NSLog(@"Test '%@'", [testNode1 selfmlRepresentation]);
	
	[pool drain];
	return 0;
}