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
extern unsigned long make_table ();
extern int add_row (unsigned long oid);
extern int add_column (unsigned long oid, char* label);
extern char* value_at (unsigned long oid, char* column_label, int row_index);
extern int put_value_at (unsigned long oid, char* column_label, int row_index, char* value);
extern int mark_column_deleted (unsigned long oid, char* column_label, int deleted);
extern int mark_row_deleted (unsigned long oid, int row_index, int deleted);
extern int show_deleted (unsigned long oid);
extern int set_show_deleted (unsigned long oid, int show);
extern int compact_table (unsigned long oid);
extern char* sort_column (unsigned long oid);
extern int set_sort_column (unsigned long oid, char* column_label);
extern int sort_order (unsigned long oid);
extern int set_sort_order (unsigned long oid, int order);
extern int sort_type (unsigned long oid);
extern int set_sort_type (unsigned long oid, int type);
extern char* filter_text (unsigned long oid);
extern int set_filter_text (unsigned long oid, char* text);
extern int write_delectus (unsigned long oid, char* path);
extern unsigned long read_delectus (char* path);
extern int write_delectus_csv (unsigned long oid, char* path);
extern unsigned long read_delectus_csv (char* path);

