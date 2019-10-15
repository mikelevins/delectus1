# Delectus 2 data

## Objects

### User

| field       | type               | description                                |
| -----       | ----               | -----------                                |
| id          | string             | A UUID identifying the account             |
| type        | string             | `"delectus-user"`                          |
| username    | string             | The user-chosen name for the account       |
| enabled     | Boolean            | Whether the account is enabled or disabled |
| collections | list of Collection | the collections that the user account owns |

### Collection

| field   | type         | description                              |
| -----   | ----         | -----------                              |
| id      | string       | A UUID identifying the collection        |
| type    | string       | `"delectus-collection"`                  |
| name    | string       | The user-defined name of the collection  |
| lists   | list of List | The collection's members (Lists)         |
| deleted | Boolean      | whether the collection is marked deleted |

### List

| field   | type                               | description                           |
| -----   | ----                               | -----------                           |
| id      | string                             | A UUID identifying the list           |
| type    | string                             | `"delectus-list"`                     |
| name    | string                             | The user-defined name of the list     |
| columns | Map from Column.id to Column.label | The Columns in each Row of the List   |
| rows    | map from Row.id to Row             | The rows that make up the list's data |
| deleted | Boolean                            | whether the list is marked deleted    |

### Column

| field    | type    | description                                                                            |
| -----    | ----    | -----------                                                                            |
| id       | integer | An integer, unique per List, identifying the Column                                    |
| type     | string  | `"delectus-column"`                                                                    |
| datatype | string  | The preferred type of values in the Column                                             |
| default  | value   | The default value provided for a field of the Column when the user has not asigned one |
| label    | string  | The user-defined label of the Column                                                   |
| deleted  | Boolean | whether the Column is marked deleted                                                   |

### Row

| field  | type                        | description                                      |
| -----  | ----                        | -----------                                      |
| id     | string                      | An integer, unique per List, identifying the Row |
| type   | string                      | `"delectus-row"`                                 |
| fields | map from Column.id to value | one value per Column                             |

## Values and Datatypes

A List stores rows of **values**.

**Datatypes** are the preferred types of values stored in lists. Each
Row has one field per Column, and each field contains a value. Each
value is an instance one of the defined datatypes.

Each Column specifies a default value for its fields. The default
value must be an instance of the preferred datatype for the Column. If
no default value is assigned when the column is Created, then a
default default value :) is assigned to the Column, as specified
below.

The defined datatypes are:

| type name | representation type | default value            | description                  |
| -----     | ----                | ----                     | -----------                  |
| NULL      | `NULL`              | `null`                   | the null value               |
| Boolean   | `bool`              | `false`                  | true or false                |
| Number    | `number`            | `0`                      | numeric values               |
| Range     | `object`            | `{min: 0, max: 0}`       | numbers in a defined range   |
| Text      | `string`            | `""`                     | text strings                 |
| Timestamp | `string`            | `"1970-01-01T00:00:00Z"` | timestamps in ISO8601 format |

