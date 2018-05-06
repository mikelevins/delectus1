# Delectus Design
2.0

The Delectus application manages **lists** of **items** that belong to
**users**. Users can organize their lists into **collections**.

Delectus refers to both lists and collections as **objects**.

By default, all of a user's collections and lists are **private**,
meaning that no one but the owning user can see or interact with
them. The owner can, however, grant **permissions** to other users to
view or edit particular lists of collections. A user can also
**publish** a list or collection, making it publicly visible (but
read-only).

The owner of a list can also enable a **comments** list for any list
or collection, and can control who can add and edit comments.

## Lists and items

A **list** is a named sequence of **items**. An **item** is a list of
**key/value pairs**. Each item belongs to exactly one list. No items
are shared by more than one list, and there are no items that are not
members of lists.

A **key** is a string used to identify a value in an item.

A **value** is a string, a number, a boolean, or an object of type
`Collection`, `List`, `User`, `Link` or `Time`.

All items in the same list have the same set of keys. A list is
therefore conceptually like a relational table, where the keys are
columns and the items are rows.

Each item has, in addition to its user-defined keys, a unique
identifier string that identifies the item itself, and one that
identifies the list that it belongs to.

Each list has a name and a text note. The name is meant to be a
human-readable piece of text that identifies the list for users. The
note may be empty, or may contain any text the owner
desires. Commonly, it's used to describe something about the contents
of the list.

Each list has a unique identifier string that is used by items and
collections to refer to the list. The identifier enables programs to
distinguish one list from another.

## Collections

A **collection** is a named object that contains zero or more
references to lists and other collections. Each user has at least one
collection, named **`delectus:Everything`**.

Every list, and every collection except `delectus:Everything`, belongs
to exactly one collection. `delectus:Everything` belongs to no
collection.

By default, when a user creates a list or a collection, it belongs to
Everything. A user can subsequently move the new list or collection
into another collection.

### Special Lists and Collections

Special Lists and Special Collections are objects that behave in most
ways like lists and collections, but which also have some special
behaviors or restrictions. Delectus uses Special Lists and Collections
to control access to your account and data, and to determine how they
behave. The following sections describe some of the uses of Special
Lists and Collections.

## Sharing, Publishing, and Permissions

Each list and collection can be **private**, **shared**, or
**published**.

### Private collections

A **private** list or collection is visible and editable only to its
owner. Lists and collections are private by default when they are
created. An owner must take deliberate action to share or publish
them.

### Shared collections

To share an object, add one or more Delectus users to the object's
`delectus:Viewers` list. All users on the `delectus:Viewers` list can then see the
object and its contents.

To enable a viewer to make changes to an object, add the viewer to the
object's `delectus:Editors` list.

You cannot add a user to `editors` without also adding them to
`delectus:Viewers`. Removing a user from `viewers` automatically
removes them from `editors` as well.

### Published collections

To publish an object, add the Special List `delectus:Everyone` to the
`delectus:Viewers` list. The object then becomes visible read-only to
anyone with a web browser.

## Comments

You can add comments to any collection, list, or item that you own.

When you share an object, you can also enable others to add comments
by adding viewers to the object's `delectus:Commenters` list. As with
the `delectus:Editors` list, if you remove a user from the
`delectus:Viewers` list, then the user loses the ability to add
comments. Delectus continues to store any comments the user already
added, though, until you deliberately remove them.

Similarly, you can enable comments for published objects in the same
way. You can't add `delectus:Everyone` to an object's
`delectus:Editors` list, though, so you can't publish a list for
everyone in the world to edit.

