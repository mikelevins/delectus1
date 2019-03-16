//
//  Delect.m
//  Delect
//
//  Created by mikel evins on 3/16/19.
//  Copyright © 2019 Delectus. All rights reserved.
//

#define ___VERSION 408009
#include "gambit.h"

#define SCHEME_LIBRARY_LINKER ____20_Delectus__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE


#include "Delect.h"

void initDelect(void) {
    ___setup_params_struct setup_params;
    
    ___setup_params_reset (&setup_params);
    
    setup_params.version = ___VERSION;
    setup_params.linker  = SCHEME_LIBRARY_LINKER;
    
    ___setup (&setup_params);
    

}

void cleanupDelect(void){
    ___cleanup ();
}
