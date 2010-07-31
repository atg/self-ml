#import "self-ml.h"

int main (int argc, const char * argv[]) {
    
	//SFNodeRef node = SFNodeCreateFromString("(foo ({#  {# `b``ar` #} # hello world `cru``nchy` \n bacon) (gaz wobble))");
	//SFNodeRef node = SFNodeCreateFromString("(grandparent (parent child1 child2) (aunt))\n(grandparent (parent child1 child2 child3))");
    //SFNodeRef node = SFNodeCreateFromString("# This is a line comment at the start of the document\n\n({# This is a block comment with {# a nested block comment#}#}abc)\n\n(# This is a line comment just after a bracket\ndef)\n\n(foo{# bar #}bar)\n\n(ghi){#{##}#}(jkl)\n\n(mno)# This is a line comment at the end of the document");
	SFNodeRef node = SFNodeCreateFromString("[This document tests all kinds of strings]\n\n`back `` ticks`\n\nverbatim-strings\n\n[Strings with # signs in them]\n[Strings with [nested brackets] in them]\n[Strings with all # kinds ` of [shit] (like) {this} {# Should #} still work]\n`As should [strings with back `` (ticks) {like} this`");
	
	SFNodePrintRepresentation(node);
	printf("\n");
	    
    return 0;
}
