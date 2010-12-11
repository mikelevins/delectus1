/*
 *  delectus.c
 *  Delectus
 *
 *  Created by mikel on 12/10/10.
 *  Copyright 2010 __MyCompanyName__. All rights reserved.
 *
 */

#include "delectus.h"

// TODO: implement delectus functions

void toggle_column_deleted(int documentID, char* label){}

void toggle_row_deleted(int documentID, int rowIndex){}

void advance_sort(int documentID, char* label){}

signed char are_deleted_items_shown(int documentID){
	return 1;
}

signed char is_column_deleted(int documentID,char* label){
	return 0;
}

signed char is_row_deleted(int documentID, int rowIndex){
	return 0;
}

signed char has_deleted_items(int documentID){
	return 0;
}

int get_column_width(int documentID, char* label){
	return 1;
}

void* value_for_cell(int documentID, char* label, int rowIndex){
	return (void*)0;
}

int get_new_document(){
	return 0;
}

signed char is_duplicate_label(int documentID, char* label){
	return 0;
}

int number_of_rows(int documentID){
	return 1;
}

int number_of_columns(int documentID){
	return 1;
}

void toggle_show_deleted(int documentID){}

void set_filter(int documentID, char* label){}

