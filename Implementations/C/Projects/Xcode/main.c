#import "self-ml.h"

int main (int argc, const char * argv[]) {
    
	
    printf("Starting \n");
    fflush(stdin);
	SFNodeRef node = SFNodeCreateFromString("(foo (`b``ar` `cru``nchy` bacon) (gaz wobble))");
    SFNodePrintRepresentation(node);
    printf("Stopping \n");
    fflush(stdin);
    
    return 0;
}
