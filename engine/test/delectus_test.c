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
  printf("  2. Make a new document");
  printf("\n");
  unsigned long oid = make_document();
  printf("  Test document id = %d",oid);
  printf("\n\n");

  /* 3. Add a "Name" column */
  printf("  3. Add a 'Name' column");
  printf("\n");
  char* name_label = "Name";
  int add_col_err = add_column(oid,name_label);
  printf("  add_column(oid, name_label)\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",add_col_err);
  printf("\n\n");

  /* 4. Add a row */
  printf("  4. Add a row");
  printf("\n");
  int add_row_err = add_row(oid);
  printf("  add_row(oid)\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",add_row_err);
  printf("\n\n");
  
  /* 5. Fetch the value of the sole row and column (should be null) */
  printf("  5. Fetch the value of the sole row and column (should be null)");
  printf("\n");
  char* val1 = value_at(oid, name_label, 0);
  printf("  value_at(oid, name_label, 0)\n");
  printf("  expected: null\n");
  printf("  observed: %s\n",val1);
  /* TODO: need to deallocate the returned string here */
  printf("\n\n");
  
  /* 6. Set the value of the sole row and column (should be null) */
  printf("  6. Set a value");
  printf("\n");
  int put_value_err = put_value_at(oid, name_label, 0, "Fred");
  printf("  put_value_at(oid, name_label, 0, \"Fred\")\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",put_value_err);
  printf("\n\n");
  
  /* 7. Fetch the value of the sole row and column (should be "Fred") */
  printf("  7. Fetch the value of the sole row and column (should be \"Fred\")");
  printf("\n");
  char* val2 = value_at(oid, name_label, 0);
  printf("  value_at(oid, name_label, 0)\n");
  printf("  expected: Fred\n");
  printf("  observed: %s\n",val2);
  /* TODO: need to deallocate the returned string here */
  printf("\n\n");
  
  /* 8. Mark the "Name" column deleted */
  printf("  8. Mark the \"Name\" column deleted");
  printf("\n");
  int mark_col_err1 = mark_column_deleted(oid, name_label, 1);
  printf("  mark_column_deleted(oid, name_label, 1)\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",mark_col_err1);
  printf("\n\n");
  
  /* 9. Mark the "Name" column undeleted */
  printf("  9. Mark the \"Name\" column undeleted");
  printf("\n");
  int mark_col_err2 = mark_column_deleted(oid, name_label, 0);
  printf("  mark_column_deleted(oid, name_label, 0)\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",mark_col_err2);
  printf("\n\n");
  
  /* 10. Check whether to show deleted items */
  printf("  10. Check whether to show deleted items");
  printf("\n");
  int whether_to_show1 = show_deleted(oid);
  printf("  show_deleted(oid)\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",whether_to_show1);
  printf("\n\n");
  
  /* 11. Set show deleted items to true */
  printf("  11. Set show deleted items to true");
  printf("\n");
  int show_error = set_show_deleted(oid, 1);
  printf("  set_show_deleted(oid, 1)\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",show_error);
  printf("\n\n");
  
  /* 12. Check whether to show deleted items */
  printf("  12. Check whether to show deleted items");
  printf("\n");
  int whether_to_show2 = show_deleted(oid);
  printf("  show_deleted(oid)\n");
  printf("  expected: 1\n");
  printf("  observed: %d\n",whether_to_show2);
  printf("\n\n");
  
  /* 13. Read CSV */
  printf("  13. Read a large CSV file");
  printf("\n");
  char* path = "/Users/mikel/Projects/delectus/test-data/zipcode.csv";
  unsigned long oid1 = read_delectus_csv(path);
  printf("  read_delectus_csv(\"%s\")\n",path);
  printf("  expected: 2\n");
  printf("  observed: %d\n",oid1);
  printf("\n\n");

  char* city_label = "city";
  char* val3 = value_at(oid1, city_label, 101);
  printf("  value_at(oid, city_label, 101)\n");
  printf("  expected: Ponce\n");
  printf("  observed: %s\n",val3);
  /* TODO: need to deallocate the returned string here */
  printf("\n\n");

  /* 14. Set the sort */
  printf("  14. Set the sort (column,order, type)");
  printf("\n");
  int sort_error1 = set_sort_column(oid1, city_label);
  printf("  set_sort_column(oid1, city_label)\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",sort_error1);
  printf("\n");
  int sort_error2 = set_sort_order(oid1, SORT_ASCENDING);
  printf("  set_sort_order(oid1, SORT_ASCENDING)\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",sort_error2);
  printf("\n");
  int sort_error3 = set_sort_type(oid1, SORT_ALPHABETICAL);
  printf("  set_sort_type(oid1, SORT_ALPHABETICAL)\n");
  printf("  expected: 0\n");
  printf("  observed: %d\n",sort_error3);
  printf("\n");
  char* val4 = value_at(oid1, city_label, 0);
  printf("  value_at(oid1, city_label, 0)\n");
  printf("  expected: Ponce\n");
  printf("  observed: %s\n",val4);
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
