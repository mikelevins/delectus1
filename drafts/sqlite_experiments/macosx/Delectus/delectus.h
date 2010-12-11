/*
 *  delectus.h
 *  Delectus
 *
 *  Created by mikel on 12/10/10.
 *  Copyright 2010 mikel evins. All rights reserved.
 *
 */

void toggle_column_deleted(int documentID, char* label);
void toggle_row_deleted(int documentID, int rowIndex);
void advance_sort(int documentID, char* label);
signed char are_deleted_items_shown(int documentID);
signed char is_column_deleted(int documentID,char* label);
signed char is_row_deleted(int documentID, int rowIndex);
signed char has_deleted_items(int documentID);
int get_column_width(int documentID, char* label);
void* value_for_cell(int documentID, char* label, int rowIndex);
int get_new_document();
signed char is_duplicate_label(int documentID, char* label);
int number_of_rows(int documentID);
int number_of_columns(int documentID);
void toggle_show_deleted(int documentID);
void set_filter(int documentID, char* label);
