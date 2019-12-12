# Delectus 2 Storage Model

## Introduction

Delectus 2 stores user data in a Couchbase database. Couchbase offers
a document database with automatic revision-tracking and support for
network syc with automated conflict resolution. These features enable
Couchbase to maintain representations of Delectus user data in a way
that offers users reliable storage that automatically keeps track of
their changes and synchronizes them across devices.

This document describes Delectus user-visible data objects and how
they are stored in Couchbase.

## User Data Model

The Delectus 2 **user data model** describes 4 kinds of objects:

- **Lists**
- **Items**
- **Columns**
- **Collections**

### Lists

The fundamental concept is the **list**: Delectus is a **list
manager**. A list is a named, ordered sequence of **items**. The user
may rename the list, may and remove items, and may change the contents
of items at any time.

### Items

The **item** is the basic element of content in a list. An item is an
ordered sequence of one or more **fields**. Each **field** contains a
**value**.

A **value** may be  any of:

- a text string
- a number
- true or false
- `NULL` (no value at all)

### Columns

All of the items in a given list contain the same number of fields. The fields are organized into **Columns**. A **Column** is a named object that represents all of the fields at a given position in a list's items.

To clarify, suppose we have a list of movies. It contains 4 columns:
one each for a movie's title, director, star, and a catalog number.

Such a list might look like this:

**Movies**

| Title | Director | Star | Number |
| ----- | -------- | ---- | ------ |
| Bell, Book and Candle | Richard Quine | Kim Novak | 1 |
| Snow White | David Hand | Adriana Caselotti | 2 |
| The Wizard of Oz | Victor Fleming | Judy Garland | 3 |

"Movies" is the **name** of the **list**.

"Title," "Director," "Star," and "Number" are the names of its four
**columns**. The first three contain text-string **values**; the
fourth contains numbers. To be clear, the column name "Number" doesn't
force you to store numbers in the column; it's just a name. You can
change it to any other name, and in fact you can store any valid type
of data in any column.

This list contains three **items**, with values 1, 2, and 3 in the "Number" column.

## Collections

A **collection** is a named, ordered sequence of lists. You can think
of a collection as a list of lists, or maybe as a folder of lists. A
new user account comes with an already-created collection named
"Default Collection". The user can create as many collections as
needed, and can delete any of them at any time. (You can even delete
the "Default Collection", but it will then be automatically recreated
if you create any lists.)

A collection has a name and a sequence of zero or more lists.

A list is always a member of exactly one collection. Lists are created
as members of the default collection, but the user may move any list
to any other collection at any time.

## Storage Model

The user data model is a conceptual outline of how Delectus user data
is structured. The **storage model** is a precise description of what
we actually store in Couchbase to support the user data model. The
greatest difference between the two is that the storage model stores
many additional pieces of data in order to make the user data model
work reliably and support pleasant user interfaces.

### Couchbase Basics

Couchbase is a **document database**, meaning that it stores
loosely-structured chunks of data known as **documents**. In the case
of Couchbase, these documents are mainly stored as **JSON objects**,
though Couchbase is also able to store arbitrary binary blobs.

Delectus 2 uses JSON objects to represent the conceptual objects from
the user data model. The actual JSON objects stored in Couchbase
contain more than just the objects described in the user data model;
the extra data support varous operations that Delectus needs to
perform in order to provide a nice interface, such as searching,
sorting, renaming, and so on.

#### List objects

A list object looks like the following in JSON form:

```
{
  "id": ...",
  "type": "delectus_list"
  "name": ...",
  "columns": ...,
  "items": ...,
  "owner-id": ...",
  "deleted": ...,
}
```

Its keys have the following meanings:

- id: a UUID that identifies this particular list
- type: the string `"delectus_list"`, identifying this object as a list
- name: the name given by the user to this list
- columns: an object containing descriptions of the list's columns
- items: an object containing the list's items and their fields
- owner-id: a UUID that identifies the user account that owns the list
- deleted: true if the user has marked the list for deletion; false otherwise

The id, type, and owner-id are immutable; they may not be changed.

The user may edit the other fields at any time using the Delectus
UI.

##### `"name"`

The `"name"` field is the name the user gave to the list. It's a text
string whose only restrictions are that the text must be something
that the Delectus UI can reliably handle and present, and it must be
unique among the user's lists--no two lists belonging to the same user
may have the same name.

##### `"columns"`

The `"columns"` field contains a JSON object that describes the list's
columns. A "columns" object might look like this:

```
{
  "0": {
    "deleted": false,
    "id": "0",
    "name": "Item"
  }
}
```

This "columns" object contains a single column named "Item" with an ID
of zero and the name "Item".

Each column gets a different ID, assigned when it's
created. The first column created gets the ID "0", the second gets the
ID "1", and so on. 

The name of the column is a text string assigned by the user. The user
may use any text, and may change the name at any time. The only
restriction is that no two columns in the same list may have the same
name at the same time.

The ID of a column never changes, but the user may change the name at
any time.

##### `"items"`

The `"items"` field contains a JSON object that describes the list's
items. An "items" object might look like this:

```
{
  "0": {
    "deleted": false,
    "fields": {
      "0": "My random item"
    },
  "id": "0"
  }
}
```

This "items" object contains a single item, item "0" with id "0". Item
"0" is not deleted. It contains a single field in column "0", which
contains the text value "My random item".

Each item gets a different ID, assigned when it's
created. The first item created gets the ID "0", the second gets the
ID "1", and so on. 

The number of an item's fields is formally required to be the same as
the number of the list's columns. Because users may add and removed
columns at any time, it's possible for the actual number of fields in
a particular item to differ from the number of the list's columns. We
handle that discrepancy with some rules about how fields are
referenced.

If a user adds a new column, then existing items will not have fields
corresponding to that column. In that case, Delectus returns `NULL`
for references to the new column in those items. If the user stores
new values to a nonexistent field, Delectus creates the field.

If a user deletes a column, Delectus changes the column's "deleted"
value to `true`, but does not change the contents of any items. The
data in fields belonging to the deleted column remain in the database,
but Delectus doesn't returnor display them in the user interface
unless the user specifically asks to see deleted data.


