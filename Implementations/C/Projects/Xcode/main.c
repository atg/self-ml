#import <stdio.h>
#import "self-ml.h"

int main (int argc, const char * argv[]) {
    
	//SFNodeRef node = SFNodeCreateFromString("(foo ({#  {# `b``ar` #} # hello world `cru``nchy` \n bacon) (gaz wobble))");
	//SFNodeRef node = SFNodeCreateFromString("(grandparent (parent child1 child2) (aunt))\n(grandparent (parent child1 child2 child3))");
    //SFNodeRef node = SFNodeCreateFromString("# This is a line comment at the start of the document\n\n({# This is a block comment with {# a nested block comment#}#}abc)\n\n(# This is a line comment just after a bracket\ndef)\n\n(foo{# bar #}bar)\n\n(ghi){#{##}#}(jkl)\n\n(mno)# This is a line comment at the end of the document");
	//SFNodeRef node = SFNodeCreateFromString("[This document tests all kinds of strings]\n\n`back `` ticks`\n\nverbatim-strings\n\n[Strings with # signs in them]\n[Strings with [nested brackets] in them]\n[Strings with all # kinds ` of [shit] (like) {this} {# Should #} still work]\n`As should [strings with back `` (ticks) {like} this`");
	
	/* SFNodeRef node = SFNodeCreateFromString("# self-ml\n# s-expression like format... markup language\n\n# self-ml is a structural"
											" data language, akin to XML and JSON.\n# Unlike XML and JSON, it is designed explicitly"
											" to be written by humans and read by computers.\n# self-ml is inspired by Lisp's s-expressions."
											"\n\n# For a detailed description, see http://dequechair.com/self-ml\n\n\n### SYNTAX ###\n# "
											"This is a comment, obviously\n\n{# This is a block comment\n   it can do this! #}\n\n# Nodes "
											"start with a \"head\" and can have zero or more children\n(fruit Apple Banana Orange)\n\n# You "
											"can nest nodes, and yes you can have more than one top-level node\n# Despite the formatting, "
											"whitespace and newlines are completely ignored, except as a separator\n(food\n    (fruit Apple"
											" Banana Orange)\n    (veggies Potato Carrot Onion))\n\n# There's one catch, strings can only be"
											" one word.\n# Kidding! Use [ ] to introduce a string literal. You can put anything in there, *ex"
											"cept* unbalanced square brackets\n(cakes\n    [Chocolate Cake]\n    [Carrot Cake]\n    [Christma"
											"s Cake])\n\n# The other style of string literal uses backticks. If you need to insert a literal ba"
											"cktick into a backtick-delimited string, double it up\n(regexen\n    `[a-z\\[0-9]+`\n    `[a-z``0"
											"-9]`)\n\n# You can have a node with no children\n(booleans\n    (true)\n    (false))\n\n# Nodes with no children are NOT EQUIVALENT to literal strings. For example, this is considered different\n(booleans\n    true\n    false)\n\n# As a general rule, heads should be part of a finite set of expected types, whereas strings may contain any valid data\n# If you have something like this, you're probably doing something wrong\n(parent (child1 foo bar) (child2 foo bar) (child3 foo bar))\n");
	
	*/
	
	SFNodeRef node;
	
	if (argc == 3 && strcmp(argv[1], "-c") == 0)
	{
		node = SFNodeCreateFromString((char *)argv[2]);
	}
	else if (argc == 2)
	{
		node = SFNodeCreateFromFile(fopen(argv[1], "r"));
	}
	else if (argc <= 1)
	{
		node = SFNodeCreateFromFile(stdin);
	}
	else
	{
		printf("Too many arguments to self-ml\n");
		return 0;
	}
	
	SFNodePrintRepresentation(node);
	    
    return 0;
}
