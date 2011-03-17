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
	char* zip_csv="/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv";
	int d2 = read_delectus_csv(zip_csv);
	printf("read_delectus_csv() -> %d\n",d2);
    char* val1 = value_at(d2,"city",0);
	printf("value_at(d2,city,0) -> %s\n",val1);
    char* testval1 = "Frob City";
    int result1 = put_value_at(d2,"city",0,testval1);
	printf("put_value_at(d2,city,0,\"Frob City\") -> %d\n",result1);
    char* testval2 =  value_at(d2,"city",0);
    printf("value_at(d2,city,0) -> %s\n\n",testval2);
    int result2 = mark_row_finished(d2,0,1);
    printf("mark_row_finished(d2,0,YES) -> %d\n\n",result2);
    int testval3 = is_row_finished(d2,0);
    printf("is_row_finished(d2,0) -> %d\n\n",testval3);

    
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
