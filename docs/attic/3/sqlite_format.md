# Delectus 2 SQLite file format

How SQLite is used in Delectus 2

## Lists

Let's suppose we have a list whose identifier is
"DEAC9B3EA26A1F159F92D42E09BC39B1".

There is a table named "DEAC9B3EA26A1F159F92D42E09BC39B1" with this
structure:

| _rowid_ | deleted? | Item | ... |
| ------- | -------- | ---- | --- |
|       1 |        0 |  ... | ...

The _rowid_ column is automatically maintained by SQLite. Each row
added receives a monotonically-increasing integer, starting with 1 and
increasing by one for each row added.

The deleted? column records whether the row has been marked
deleted. If the value in the deleted? column is 0 then the row has not
been deleted; otherwise it should be 1, and the row has been deleted.

The column named "Item" is the default column for user-defined
contents. Values in the column are assumed to be text strings. The
user may rename the "Item" column and change its value type, and may
add as many additional columns as he or she likes.

The column marked "..." stands for user-defined columns, of which
there may be any number. Each one may have any name that obeys the
SQLite rules for column naming.

There is an auxiliary table named "DEAC9B3EA26A1F159F92D42E09BC39B1
metadata" with the following structure:

| _rowid_ | deleted? | Column | type   |
| ------- | -------- | ------ | ------ |
|       1 |        0 | label0 | string |
|       2 |        0 | label1 | number |
|     ... |      ... |    ... |    ... |

The table "DEAC9B3EA26A1F159F92D42E09BC39B1 metadata" describes the
columns that appear in the list table
"DEAC9B3EA26A1F159F92D42E09BC39B1". It gives the labels of the columns
in the list table, the types associated with values in the column, and
whether each column has been marked deleted.

There is a second auxiliary table named
"DEAC9B3EA26A1F159F92D42E09BC39B1 changes" with a structure like this:

| _rowid_ | execute order | message        | timestamp       | hash         |
| ------- | ------------- | -------------- | --------------- | ------------ |
|       1 |             1 | change-message | ISO-8601 string | an sha3 hash |

This table contains the change log for the List. Any historical state
of the list can be reproduced by starting with a newly-created, empty
table, and applying the change messages in execute order. When two
copies of a Delectus list are synchronized, their chagelogs are
merged. The merged changelogs are then used to reconstruct the Lists,
and the merged changelogs then replace the contents of this table.

Finally, there is a row in the special table named "Directory" with
the following structure:

| _rowid_ | deleted? | identity                         | title   | type  | owner  |
| ------- | -------- | -------------------------------- | ------- | ----- | ------ |
|       1 |        0 | DEAC9B3EA26A1F159F92D42E09BC39B1 | My List | List  | userid |

## Collections

A Collection is represented as a single table, structured like this:

| _rowid_ | deleted? | Object    | title        | type       |
| ------- | -------- | --------- | ------------ | ---------- |
|       1 |        0 | object-id | A List       | List       |
|       2 |        0 | object-id | A Collection | Collection |

Collections may contain either Lists, Collections, or a mix of
both. Each object-id is an identity token, similar to the one shown in
the List section.

Each collection appears in the Directory table, like this:

| _rowid_ | deleted? | identity                         | title         | type        | owner  |
| ------- | -------- | -------------------------------- | ------------- | ----------- | ------ |
|       1 |        0 | DEAC9B3EA26A1F159F92D42E09BC39B1 | My Collection | Collection  | userid |

## User accounts

## The Directory

