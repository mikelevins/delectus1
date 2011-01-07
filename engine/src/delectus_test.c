#include <stdio.h>
#include <stdlib.h>

/**********************************************************************/
/* Prelude: set up Gambit runtime                                     */
/**********************************************************************/

/*
 * ___VERSION must match the version number of the Gambit header file.
 */

#define ___VERSION 406000
#include "gambit.h"

/*
 * Include declarations exported by server.
 */

#include "delectus.h"

/*
 * Define SCHEME_LIBRARY_LINKER as the name of the Scheme library
 * prefixed with "____20_" and suffixed with "__".  This is the
 * function that initializes the Scheme library.
 */

#define SCHEME_LIBRARY_LINKER ____20_delectus__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE

int main (int argc, char **argv)
{
  char *temp;

  /*
   * Setup the Scheme library by calling "___setup" with appropriate
   * parameters.  The call to "___setup_params_reset" sets all parameters
   * to their default setting.
   */

  ___setup_params_struct setup_params;

  ___setup_params_reset (&setup_params);

  setup_params.version = ___VERSION;
  setup_params.linker  = SCHEME_LIBRARY_LINKER;

  ___setup (&setup_params);

/**********************************************************************/
/* Delectus tests                                                     */
/**********************************************************************/

  printf("\nDelectus test program\n");
  
/**********************************************************************/
/* Postlude: tear down Gambit runtime                                 */
/**********************************************************************/

  fflush (stdout);

  /* Cleanup the Scheme library */

  ___cleanup ();

  return 0;
}
