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

The Delectus 2 **user data model** describes 5 kinds of objects:

- **Lists**
- **Items**
- **Columns**
- **Collections**
- **User Accounts**

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

### User Accounts

A **user account** is an object that describes a Delectus user and
enables the user to log in to the application. In order to browse and
edit Delectus data, a user must first log in to a valid account.

Each user account has a unique identifier, an email address, and a
password. An account may be enabled or disabled; only a Delectus
administrator may change the enabled status of an account.

All lists and collections are owned by user accounts. A user account's
identifier is used to identify the owner of a list or collection.

## Storage Model

The user data model is a conceptual outline of how Delectus user data
is structured. The **storage model** is a precise description of what
we actually store in Couchbase to support the user data model. The
greatest difference between the two is that the storage model stores
many additional pieces of data in order to make the user data model
work reliably and support pleasant user interfaces.

### Couchbase Basics

Couchbase is a **document databasemanager**, meaning that it stores
loosely-structured chunks of data known as **documents**. In the case
of Couchbase, these documents are mainly stored as **JSON objects**,
though Couchbase is also able to store arbitrary binary blobs.

Delectus 2 uses JSON objects to represent the conceptual objects from
the user data model. The actual JSON objects stored in Couchbase
contain more than just the objects described in the user data model;
the extra data support varous operations that Delectus needs to
perform in order to provide a nice interface, such as searching,
sorting, renaming, and so on.

Delectus stores its data in two different databases (Couchbase calls
them "buckets").

The **"delectus_content"** database stores lists and collections.

The **"delectus_users"** database stores user-account data.

### Object identity

Each list and collection object has an ID. The ID is a unique UUID
string that no other object has. The user interface provides no way
for a user to choose or change this UUID; it is assigned by Delectus
when the list or collection is created, and remains the same
throughout its existence.

Within a list, items and columns have ID strings, too. These IDs are
not UUIDs. They are required to be unique only within the list that
contains them. Item and column IDs are of the form "0", "1". "2", and
so on. The ID is assigned when an item or column is created, and it
remains the same for the life of the item or column; there is no
support for changing it.

Within a list item, each field has an ID that it shares with the
column that the field belongs to. A field whose ID is "0" belongs to
column "0"of its list; field "1" belongs to column "1", and so
on. Field IDs are unique within an item.

As with collections, lists, and columns, the ID of a field is assigned
when the field is created and remains the same throughout the life of
the field.

### List Objects

A list object looks like the following in JSON form:

```
{
  "id": ...",
  "type": "delectus_list"
  "owner-id": ...",
  "deleted": ...,
  "name": ...",
  "columns": ...,
  "items": ...,
}
```

Its keys have the following meanings:

- id: a UUID that identifies this particular list
- type: the string `"delectus_list"`, identifying this object as a list
- owner-id: a UUID that identifies the user account that owns the list
- deleted: true if the user has marked the list for deletion; false otherwise
- name: the name given by the user to this list
- columns: an object containing descriptions of the list's columns
- items: an object containing the list's items and their fields

The id, type, and owner-id are immutable; they may not be changed.

The user may edit the other fields at any time using the Delectus
application.

##### `"deleted"`

The `"deleted"` field contains either true or false: true if the user
has marked the list for deletion; false otherwise. The user may mark
or unmark a list at any time.

##### `"name"`

The `"name"` field is the name the user gave to the list. It's a text
string whose only restrictions are that the text must be something
that the Delectus UI can reliably handle and present, and it must be
unique among the user's lists--no two lists belonging to the same user
may have the same name. The user may change a list's name at any time.

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

The user may add columns at any time. They may set the "deleted"
attribute to true or false at any time.

Aside from the abovementioned changes, the user may not change the
structure of the "columns" object.

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

Users may add items at any time. They may set the "deleted" attribute
to true or false at any time.

They may change the contents of any field in the "fields" object, but
may not change the fields object in any other way, except indirectly,
by adding or removing columns in the "columns" object.

Users may not change the ID of an item or otherwise affect the
structure of the "items" object.

#### Items and deleted columns

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


### Collection Objects

A collection object looks like the following in JSON form:

```
{
  "id": ...",
  "type": "delectus_collection"
  "owner-id": ...",
  "deleted": ...,
  "name": ...,
  "lists": ...,
}
```

Its keys have the following meanings:

- id: a UUID that identifies this particular collection
- type: the string `"delectus_collection"`, identifying this object as a collection
- owner-id: a UUID that identifies the user account that owns the collection
- deleted: true if the user has marked the collection for deletion; false otherwise
- name: the name given by the user to this collection
- lists: an object containing descriptions of the collection's lists

The id, type, and owner-id are immutable; they may not be changed.

The user may edit the other fields at any time using the Delectus
application.

##### `"deleted"`

The `"deleted"` field contains either true or false: true if the user
has marked the collection for deletion; false otherwise. The user may mark
or unmark a list at any time.

When a user marks a collection deleted, the lists that it contains
become invisible in the user interface, unless the user sets a UI
preference to show deleted items. In order to make the lists visible
by default, the user must move them to another collection or unmark
the collection for deletion.

##### `"name"`

The `"name"` field is the name the user gave to the collection. It's a
text string whose only restrictions are that the text must be
something that the Delectus UI can reliably handle and present, and it
must be unique among the user's collections--no two collections
belonging to the same user may have the same name. The user may change
a collection's name at any time.

##### `"lists"`

The `"lists"` field is an object whose keys are the IDs of Delectus
lists. Each ID identifies a list that belongs to the user--users may
not put other users' lists in their collections.

Because every list muct belong to exactly one collection, creating a
list adds it to the default collection. The user may then move the
list to any other collection (that is not marked deleted). Moving a
list to a different collection removes it from the previous collection
at the same time.

It's not possible to simply remove a list from a collection. The only
options are to move the list to another collection, or to mark the
list deleted.

### User Account Objects

A user account object looks like the following in JSON form:

```
{
  "id": ...,
  "type": "delectus_user"
  "enabled": ...,
  "email": ...,
  "name": ...,
  "password-hash": ...,
}
```

Its keys have the following meanings:

- id: a UUID that identifies this particular user account
- type: the string `"delectus_user"`, identifying this object as a user account
- owner-id: a UUID that identifies the user account that owns the list
- enabled: true if the user account is active and may be used, and
  false otherwise; only a Delectus admin may change this value
- email: the email address associated with the user account. Only a
  Delectus admin may change the email.
- name: the name that the user wishes to display in the Delectus
  application. The user may set this value at any time. If it's empty
  then Delectus uses the value of the email field in place of the
  name.
- password-hash: The hashed password string usedto authenticate the
  user for logins. A user must be authenticated and logged in before
  it's possible to browse and edit Delectus data. The Delectus
  application provides an interface for updating the password, as does
  the admin application.

The id and type are immutable; they may not be changed.

