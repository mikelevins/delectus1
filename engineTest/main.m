#import <Cocoa/Cocoa.h>
#define ___VERSION 406000
#include "gambit.h"
#include "Delectus.h"
#include "stdio.h"

#define SCHEME_LIBRARY_LINKER ____20_Delectus__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE


void runTests(){
	printf("\nDelectus Engine tests\n\n");
	char* v = version();
	printf("version() -> %s\n",v);
	int d1 = new_delectus();
	printf("new_delectus() -> %d\n",d1);
	add_column(d1,"foo");
	add_row(d1);
	put_value_at(d1, "foo", 0, "apple");
	char*val=value_at(d1, "foo", 0);
	printf("test value == %s\n",val);
	BOOL yesOrNo1 = is_row_deleted(d1, 0);
	if(yesOrNo1){
		printf("is_row_deleted->YES\n");
	}else{
		printf("is_row_deleted->NO\n");
	}
	printf("marking row deleted...\n");
	int err = mark_row_deleted(d1, 0, YES);
	printf("err == %d\n",err);
	BOOL yesOrNo2 = is_row_deleted(d1, 0);
	if(yesOrNo2){
		printf("is_row_deleted->YES\n");
	}else{
		printf("is_row_deleted->NO\n");
	}
	
    
}

int main(int argc, char *argv[])
{
	___setup_params_struct setup_params;
	___setup_params_reset (&setup_params);
	setup_params.version = ___VERSION;
	setup_params.linker  = SCHEME_LIBRARY_LINKER;
	___setup (&setup_params);
	
    runTests();
	printf("\n\n");
	
//	___cleanup (); // prevents printf from flushing output
	
	return 0;
	
}
