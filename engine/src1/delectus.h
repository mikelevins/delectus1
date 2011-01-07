#define NONE         0
#define DESCENDING   1
#define ASCENDING    2
#define NUMERIC      3
#define ALPHABETICAL 4

#define NO_VALUE 0L

#define CANT_CREATE        (-1)
#define CANT_ADD_ROW       (-2)
#define LABEL_IN_USE       (-3)
#define NO_SUCH_COLUMN     (-4)
#define INDEX_OUT_OF_RANGE (-5)
#define CANT_UPDATE        (-6)
#define CANT_WRITE         (-7)
#define CANT_READ          (-8)
#define BAD_FORMAT         (-9)

extern (int) version ();
extern (unsigned long) make_table ();
extern (int) add_row (unsigned long oid);
extern (int) add_column (unsigned long oid, char* label);
extern (char*) value_at (unsigned long oid, char* column_label, int row_index);
extern (int) put_value_at (unsigned long oid, char* column_label, int row-index, char* value);
extern (int) mark_column_deleted (unsigned long oid, char* column_label, bool deleted);
extern (int) mark_row_deleted (unsigned long oid, int row_index, bool deleted);
extern (bool) show_deleted (unsigned long oid);
extern (int) set_show_deleted (unsigned long oid, bool show);
extern (int) compact_table (unsigned long oid);
extern (char*) sort_column (unsigned long oid);
extern (int) set_sort_column (unsigned long oid, char* column_label);
extern (int) sort_order (unsigned long oid);
extern (int) set_sort_order (unsigned long oid, int order);
extern (int) sort_type (unsigned long oid);
extern (int) set_sort_type (unsigned long oid, int type);
extern (char*) filter_text (unsigned long oid);
extern (int) set_filter_text (unsigned long oid, char* text);
extern (int) write_table (unsigned long oid, char* path);
extern (unsigned long) read_table (char* path);
extern (int) write_table_csv (unsigned long oid, char* path);
extern (unsigned long) read_table_csv (char* path);

