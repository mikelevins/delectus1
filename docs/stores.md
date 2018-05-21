# Delectus 2 Store Files

Delectus 2 stores data in **storefiles**. A **storefile** is a SQLite3
file with a specific internal organization. This document describes
that organization.

## Metadata

A Delectus 2 storefile is identified by a table named "Delectus2". The
table stores metadata about the storefile in key/value format.

Delectus2

| rowid | key      | value                       |
| ----- | -------- | --------------------------- |
|     1 | version  | <semantic version string>   |
|     2 | node     | <delectus identity string>  |
|     3 | storeid  | <delectus identity string>  |
|     4 | username | <user account ID>           |
|     5 | state    | <changelog hash>            |

- **version** identifies the version of the Delectus file format in
    use.

- **node** identifies the instance of the Delectus application that
    manages the store.

- **storeid** uniquely identifies the collection of data in the
    store. Two storefiles with the same storeid are considered by
    Delectus to be two copies of the same store.

- **username** identifies the Delectus user who owns the store. If the
    username is "anonymous" then the store has never been associated
    with an authenticated user account, and cannot be synchronized. If
    the username is "delectus" then the store belongs to the Delectus
    application itself.

- **state** identifies the current state of the storefile's
    changelog. When two copies of a store have different state values,
    Delectus knows that they are out of sync, and that a
    synchronization is required to bring them back into congruence.

## Directory

A Delectus 2 storefile contains List and Collection objects. Each
object is a row in a table. The **Directory** table identifies the
table that each object belongs to.

| rowid | identity                    | title          | type          |
| ----- | --------------------------- | -------------- | ------------- |
|     1 | <delectus identity string>  | <title string> | <object type> |
|   ... |                         ... |            ... |           ... |

- **identity** is a Delectus identity string that identifies the object.

- **title** is a text string assigned by the user to name the object.

- **type** is either "List" or "Collection".

## Lists

If the type of an object is "List", then it is represented by two
tables:

- the **object table** is a table whose name is the object's identity
  string. For example, if the List object's identity string is
  "DEAD6F2320071B319CFDC61F1A488B74", then that is also the name of
  the table that contains the List object.

- the **metadata table** is a table whose name is "<identity string>
  metadata". For example, if the List object's identity string is
  "DEAD6F2320071B319CFDC61F1A488B74", then the metadata table's name
  is "DEAD6F2320071B319CFDC61F1A488B74 metadata".

The **object table** contains the members of the List, one in each row
of the table, in a format like this:

| rowid | deleted? | Item | ... |
| ----- | -------- | ---- | --- |
|     1 | <0 or 1> |  ... | ... |
|   ... |      ... |  ... | ... |

In an object table, the columns "rowid" and "deleted?" are always
present. The "Item" column is created with type "string" when the List
(and the table that represents it) is created. The user may rename the
"Item" column, change its type, or mark it deleted.

The user may also add more columns, give them any name that obeys
SQLite's rules for naming columns, or mark any of them deleted.

The **metadata table** contains descriptions of the Lists' columns, in
the following format:

| rowid | column         | order          | type          | deleted? |
| ----- | -------------- | -------------- | ------------- | -------- |
|     1 | <label string> | <order number> | <value type>  | <0 or 1> |
|   ... |            ... |            ... |           ... |      ... |

## Collections

If the type of an object is "Collection" then it's represented by a
single **object table**. Like a List's object table, the Collection's
object table is named by the Collection's identity string. For
example, if a Collection's identity string is
"DEAD73286D9D1CB39C422F9C89996EB1" then that is also the name of its
object table.

Collection object tables all have the same format:

| rowid | execute order | deleted? | identity                    | title          | type          |
| ----- | ------------- | -------- | --------------------------- | -------------- | ------------- |
|     1 |             1 | <0 or 1> | <delectus identity string>  | <title string> | <object type> |
|   ... |           ... |      ... |                         ... |            ... |           ... |

## Changes

A Delectus storefile maintains a log of all changes applied to it. The
changelog is a table that records **change messages** successfully
processed by the local node on the storefile. Each change message
specifies a valid **change operation** together with parameter values
needed to yield a valid change to the store. For a complete
description of change messages, see the document "changes.md".

The only supported way to change the contents of a Delectus storefil
is by executing a change message. Because Delectus records all
processed change messages in the change log, the log contains all the
data needed to reproduce the exact contents of the storefile at any
point in its history.

Delectus uses this feature to synchronize copies of storefiles. When
asked to back up a store to the Delectus server, Delectus creates a
storefile on the server with the same storeid as the reference
storefile. It then executes the messages from the reference store's
changelog in execute order. After the last message is executed, the
new copy contains exactly the same data as the original.

Delectus follows the same procedure when replicating a store from the
Delectus server to a device--for example, when a user first logs in to
an existing account with a Delectus client application.

Finally, when Delectus detects that two copies of a store are
incongruent (that is, when their state values differ), it reconciles
the differences by merging their changelogs and then rebuilding both
copies using the merged changelogs in order to bring the two copies
back into congruence.

