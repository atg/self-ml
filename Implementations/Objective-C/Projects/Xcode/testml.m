#import <Foundation/Foundation.h>
#import <self-ml/SFONode.h>

int main (int argc, const char * argv[]) {
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	
	SFONode *testNode1 = [SFONode nodeFromString:@"(test鼀⎋鼀§¡~å˚Ωø¬z ∫ßµ鼉z)"];
	//[testNode1 addChild:SELFML(@"foo", @"bar", @"baz")];
	
	//[testNode1 setValue:@"wobble" forKey:@"foo"];
	//[testNode1 setValue:@"gaz" forKey:@"foo"];
	
	
	
	/*SFONode *testNode2 = [SFONode node];
	[testNode1 setHead:@"foo"];
	[testNode2 setHead:@"bar"];
	
	[testNode1 addChild:testNode2];
	*/
	NSLog(@"Test '%@'", [testNode1 selfmlRepresentation]);
	
	NSLog(@"[testNode1 nodeRef] = %@", [[[testNode1 children] lastObject] extractStrings]);
	SFNodePrintRepresentation([testNode1 nodeRef]);
	
	[pool drain];
	return 0;
}