# Delectus 2 data

## Objects

### User

| field | type | description |
| ----- | ---- | ----------- |
| id | string | A UUID identifying the account |
| type | string | `"user"` |
| username | string | The user-chosen name for the account |
| enabled | Boolean | Whether the account is enabled or disabled |

### Collection

| field | type | description |
| ----- | ---- | ----------- |
| id | string | A UUID identifying the collection |
| type | string | `"collection"` |
| name | string | The user-defined name of the collection |
| lists | array of id | The list of the collection's members (lists) |
| deleted | Boolean | whether the collection is marked deleted |

### List

| field | type | description |
| ----- | ---- | ----------- |
| id | string | A UUID identifying the list |
| type | string | `"list"` |
| name | string | The user-defined name of the list |
| columns | map from column id to label | The keys that appear in the list's rows |
| rows | map from row id to row | The rows that make up the list's data |
| deleted | Boolean | whether the list is marked deleted |

### Column

| field | type | description |
| ----- | ---- | ----------- |
| id | integer | An integer, unique per list, identifying the column |
| type | string | `"column"` |
| datatype | string | The preferred type of values in the column |
| default | value | The default value provided for a field of the column when the user has not asigned one |
| label | string | The user-defined label of the column |
| deleted | Boolean | whether the column is marked deleted |

### Row

| field | type | description |
| ----- | ---- | ----------- |
| id | string | An integer, unique per list, identifying the row |
| type | string | `"row"` |
| fields | map from column id to value | one value per list column |

## Values and Datatypes

A List stores rows of **values**.

**Datatypes** are the preferred types of values stored in lists. Each
row has one field per list column, and each field contains a
value. Each value is an instance one of the defined datatypes.

Each column specifies a default value for its fields. The default
value must be an instance of the preferred datatype for the column. If
no default value is assigned when the column is created, then a
default default value :) is assigned to the column, as specified
below.

The defined datatypes are:

| type name | representation type | default value | description |
| ----- | ----  | ---- | ----------- |
| NULL | `NULL` | `null` | the null value |
| Boolean | `bool` | `false` | true or false |
| Number | `number` | `0` | numeric values |
| Range | `object` | `{min: 0, max: 0}` | numbers in a defined range |
| Text | `string` | `""` | text strings |
| Timestamp | `string` | `"1970-01-01T00:00:00Z"` | timestamps in ISO8601 format |

