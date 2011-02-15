#define SORT_NONE         0
#define SORT_DESCENDING   1
#define SORT_ASCENDING    2
#define SORT_NUMERIC      3
#define SORT_ALPHABETICAL 4

#define VAL_NO 0
#define VAL_YES 1
#define VAL_NO_VALUE 0L

#define OBJ_NO_OID 0L

#define ERR_NO_ERROR            (0)
#define ERR_UNKNOWN_ERROR      (-1)
#define ERR_CANT_CREATE        (-2)
#define ERR_CANT_ADD_ROW       (-3)
#define ERR_CANT_ADD_COLUMN    (-4)
#define ERR_NO_SUCH_COLUMN     (-5)
#define ERR_INDEX_OUT_OF_RANGE (-6)
#define ERR_CANT_UPDATE        (-7)
#define ERR_CANT_WRITE         (-8)
#define ERR_CANT_READ          (-9)
#define ERR_BAD_FORMAT         (-10)
#define ERR_NO_DOCUMENT        (-11)

extern int version ();
extern int new_delectus ();
extern char* value_at (int oid, char* column_label, int row_index);
extern int put_value_at (int oid, char* column_label, int row_index, char* value);
extern int add_row (int oid);
extern int add_column (int oid, char* label);
extern int mark_column_deleted (int oid, char* column_label, int deleted);
extern int mark_row_deleted (int oid, int row_index, int deleted);
extern int include_deleted (int oid);
extern int set_include_deleted (int oid, int show);
extern int compact_delectus (int oid);
extern char* sort_column (int oid);
extern int set_sort_column (int oid, char* column_label);
extern int sort_order (int oid);
extern int set_sort_order (int oid, int order);
extern char* filter_text (int oid);
extern int set_filter_text (int oid, char* text);
extern int write_delectus_file (int oid, char* path);
extern int read_delectus_file (char* path);
extern int write_delectus_csv (int oid, char* path);
extern int read_delectus_csv (char* path);

