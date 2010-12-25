//
//  main.m
//  Hello
//
//  Created by mikel on 12/25/10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#define ___VERSION 406000
#include "gambit.h"
#include "hello.h"

#define SCHEME_LIBRARY_LINKER ____20_hello__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE


int main(int argc, char *argv[])
{
	___setup_params_struct setup_params;
	
	___setup_params_reset (&setup_params);
	
	setup_params.version = ___VERSION;
	setup_params.linker  = SCHEME_LIBRARY_LINKER;
	
	___setup (&setup_params);

    return NSApplicationMain(argc, (const char **) argv);

	___cleanup ();
}
