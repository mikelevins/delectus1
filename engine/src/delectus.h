#define delectus unsigned long
#define err int

#define NONE         0
#define DESCENDING   1
#define ASCENDING    2
#define NUMERIC      3
#define ALPHABETICAL 4

#define NO_VALUE 0L
#define NO_DELECTUS 0L

#define CANT_CREATE        (-1) /* unable to create delectus. */
#define CANT_ADD_ROW       (-2) /* unable to add row */
#define LABEL_IN_USE       (-3) /* column label is already in use */
#define NO_SUCH_COLUMN     (-4) /* no such column */
#define INDEX_OUT_OF_RANGE (-5) /* row-index out of range */
#define CANT_UPDATE        (-6) /* unable to update a delectus */
#define CANT_WRITE         (-7) /* unable to write to the supplied pathname */
#define CANT_READ          (-8) /* unable to read from the supplied pathname */
#define BAD_FORMAT         (-9) /* can't interpret supplied data */

extern (int) version ();
extern (delectus) make_delectus ();
extern (err) add_row (delectus del);
extern (err) add_column (delectus del, char* label);
extern (char*) value_at (delectus del, char* column_label, int row_index);
extern (err) put_value_at (delectus del, char* column_label, int row-index, char* value);
extern (err) mark_column_deleted (delectus del, char* column_label, bool deleted);
extern (err) mark_row_deleted (delectus del, int row_index, bool deleted);
extern (bool) show_deleted (delectus del);
extern (err) set_show_deleted (delectus del, bool show);
extern (err) compact_delectus (delectus del);
extern (char*) sort_column (delectus del);
extern (err) set_sort_column (delectus del, char* column_label);
extern (int) sort_order (delectus del);
extern (err) set_sort_order (delectus del, int order);
extern (int) sort_type (delectus del);
extern (err) set_sort_type (delectus del, int type);
extern (char*) filter_text (delectus del);
extern (err) set_filter_text (delectus del, char* text);
extern (err) write_delectus (delectus del, char* path);
extern (delectus) read_delectus (char* path);
extern (err) write_delectus_csv (delectus del, char* path);
extern (delectus) read_delectus_csv (char* path);

