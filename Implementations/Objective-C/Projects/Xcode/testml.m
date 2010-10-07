#import <Foundation/Foundation.h>
#import <self-ml/SFONode.h>

int main (int argc, const char * argv[]) {
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	
	SFONode *testNode1 = [SFONode nodeFromString:@"(zone\n        meta.preprocessor.diagnostic.c\n        (start\n            (regex [^\\s*#\\s*(error|warning)\\b]))\n        (zone\n            punctuation.separator.continuation.c\n            (match\n                (regex [(?>\\\\s*\n)])))\n        (end\n            (regex $)))"];
	/*SFONode *testNode2 = [SFONode node];
	[testNode1 setHead:@"foo"];
	[testNode2 setHead:@"bar"];
	
	[testNode1 addChild:testNode2];
	*/
	NSLog(@"Test '%@'", [testNode1 selfmlRepresentation]);
	
	NSLog(@"[testNode1 nodeRef] = %d", [testNode1 nodeRef]);
	SFNodePrintRepresentation([testNode1 nodeRef]);
	
	[pool drain];
	return 0;
}