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

  printf("\nDelectus test program");
  printf("\n\n");

  /* 1. Get the engine version */
  printf("  1. Engine version");
  printf("\n");
  int v = version();
  printf("  Engine version: %d",v);
  printf("\n\n");

  /* 2. Make a new table */
  printf("  2. Make a new table");
  printf("\n");
  unsigned long oid = make_table();
  printf("  Test table id = %d",oid);
  printf("\n\n");

  /* 3. Add a "Name" column */
  printf("  3. Add a 'Name' column");
  printf("\n");
  char* name_label = "Name";
  int add_col_err = add_column(oid,name_label);
  printf("  add_column(oid, name_label) returned %d",add_col_err);
  printf("\n\n");

  /* 4. Add a row */
  printf("  4. Add a row");
  printf("\n");
  int add_row_err = add_row(oid);
  printf("  add_row(oid) returned %d",add_row_err);
  printf("\n\n");
  
  /* 5. Fetch the value of the sole row and column (should be null) */
  printf("  5. Fetch the value of the sole row and column (should be null)");
  printf("\n");
  char* val1 = value_at(oid, name_label, 0);
  printf("  value_at(oid, name_label, 0) returned '%s' (should be nothing)",val1);
  /* TODO: need to deallocate the returned string here */
  printf("\n\n");
  
  /* 6. Set the value of the sole row and column (should be null) */
  printf("  6. Set a value");
  printf("\n");
  int put_value_err = put_value_at(oid, name_label, 0, "Fred");
  printf("  put_value_at(oid, name_label, 0, \"Fred\") returned %d",put_value_err);
  printf("\n\n");
  
  /* 7. Fetch the value of the sole row and column (should be "Fred") */
  printf("  7. Fetch the value of the sole row and column (should be \"Fred\")");
  printf("\n");
  char* val2 = value_at(oid, name_label, 0);
  printf("  value_at(oid, name_label, 0) returned '%s' (should be \"Fred\")",val2);
  /* TODO: need to deallocate the returned string here */
  printf("\n\n");
  
/**********************************************************************/
/* Postlude: tear down Gambit runtime                                 */
/**********************************************************************/

  fflush (stdout);

  /* Cleanup the Scheme library */

  ___cleanup ();

  return 0;
}
