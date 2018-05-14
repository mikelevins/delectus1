
Following are the change operations supported by the Delectus
engine. The data storage engine executes these operations when the
sync engine or the user interface requests them. In either case, the
operation changes the state of the datastore, computes a new state
token for it (a hash of the store's new state), and appends the state
token to the state log. It also adds the executed command to the
change log. Operations that update values record both the value before
the operation and the value after, so that recorded change operations
can be undone and redone.

| Name                        |
|-----------------------------|
| create-collection           |
| mark-collection-deleted     |
| destroy-collection          |
| add-list-to-collection      |
| remove-list-from collection |
| create-list                 |
| mark-list-deleted           |
| destroy-list                |
| add-column-to-list          |
| mark-column-deleted         |
| destroy-column              |
| add-row-to-list             |
| mark-row-deleted            |
| delete-row                  |
| update-field                |
