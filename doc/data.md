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
| columns | map from index to label | The keys that appear in the list's rows |
| rows | array of row | The array of rows that make up the list's data |
| deleted | Boolean | whether the list is marked deleted |

### Column

| field | type | description |
| ----- | ---- | ----------- |
| id | integer | An integer, unique per list, identifying the column |
| type | string | `"column"` |
| datatype | string | The preferred type of values in the column |
| label | string | The user-defined label of the column |
| deleted | Boolean | whether the column is marked deleted |

### Row

| field | type | description |
| ----- | ---- | ----------- |
| id | string | An integer, unique per list, identifying the row |
| type | string | `"row"` |
| fields | array of value | An array, one element per list column, of values stored in the row |

## Values and Datatypes

A List stores rows of **values**.

**Datatypes** are the preferred types of values stored in lists. Each
row has one field per list column, and each field contains a
value. Each value is an instance one of the defined datatypes.

The defined datatypes are:

- NULL: the null value
- Boolean: true or false
- Number: any numeric value
- Range: a number between a defined minimum and a defined maximum
- Text: a text string
- Timestamp: an ISO 8601 timestamp string

