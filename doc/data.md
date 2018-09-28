# Delectus Data Format

## Data Dictionary

### Database

    Database = {
      id: DatabaseID
      format: FormatIdentifier
      node: NodeID
      log: Log
      lists: [List]
      collections: [Collection]
      }

### Log

    Log = {
      id: LogID
      database: DatabaseID
      changes: [Change]
      }

    Change = {
      id: ChangeID
      origin: NodeID
      timestamp: TimeStamp
      operation: OperationName
      arguments: [OperationArgument]
      }

### Collection

    Collection = {
      id: CollectionID
      name: unique_per_database(String)
      deleted: Boolean
      lists: [List]
      }

### List

    List = {
      id: ListID
      name: String
      deleted: Boolean
      columns: [Column]
      column_order: [ColumnID]
      items: [i: Item where length(i) == length(columns)
                      and (map typeof i) == (map type: columns)]
      } 

### Column

    Column = {
      id: ColumnID
      label: ColumnLabel
      deleted: Boolean,
      type: Boolean | Number | String
      }

### Item

    Item = {
      id: ItemID
      deleted: Boolean
      fields: [Value]
      }

### Value

    Value = NULL
          | Boolean b
          | Number n
          | String s

