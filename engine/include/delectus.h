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

typedef int val;
typedef int oid;
typedef int err;

extern oid version ();
extern oid new_delectus ();
extern oid get_view(oid id, int include_deleted, int sort_column, int sort_order, char* filter_text);
extern val value_at (oid id, char* column_label, int row_index); // returns a value id
extern err put_value_at (oid id, char* column_label, int row_index, char* value);
extern err add_row (oid id);
extern err add_column (int oid, char* label);
extern err mark_column_deleted (oid id, char* column_label, int deleted);
extern err mark_row_deleted (oid id, int row_index, int deleted);
extern err compact_delectus (oid id);
extern err write_delectus_file (oid id, char* path);
extern oid read_delectus_file (char* path);
extern err write_delectus_csv (oid id, char* path);
extern oid read_delectus_csv (char* path);
extern err release_value (char* val); // releases a string malloced by Gambit
