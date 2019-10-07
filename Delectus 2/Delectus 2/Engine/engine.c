//
//  engine.c
//  Delectus 2
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

#include <stdbool.h>
#define ___VERSION 409003
#include "gambit.h"
#include "Delectus.h"

#include "engine.h"

#define SCHEME_LIBRARY_LINKER ___LNK_Delectus__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state);
___END_C_LINKAGE

int init_delectus1() {
    ___setup_params_struct setup_params;
    
    ___setup_params_reset (&setup_params);
    
    setup_params.version = ___VERSION;
    setup_params.linker  = SCHEME_LIBRARY_LINKER;
    
    long err;
    int result = ERR_NO_ERROR;
    err = ___setup (&setup_params);
    if (err != ___FIX(___NO_ERR)) {
        result = ERR_CANT_INIT_ENGINE;
    }
    return result;
}

void deinit_delectus1()
{
  ___cleanup ();
}

