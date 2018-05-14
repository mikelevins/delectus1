# Delectus 2 Data Objects

Delectus 2 manages **containers** of **collections** of **lists**. A
**list** consists of a sequence of **rows** divided into labeled
**columns**, with a **field** for each column of each row.

Along with the representation of data objects, Delectus 2 consists of
several **user interfaces** and a **data engine**, each of which can
read and update containers according to well-defined **protocols**.

This document describes the data structures stored in Delectus
containers.

## Identity

The objects that Delectus manages may be stored in more than one
location. The **sync engine** uses the **change protocol** to resolve
differences that arise in different copies of the same object.

In order for this reconciliation to work, Delectus requires a way to
identify which stored objects are copies of which other stored
objects. It solves this problem by assigning **identities** to several
types of objects.

**Containers**, **Collections**, and **Lists** are all assigned v4
UUIDs that uniquely identify them, distinguishing them from all other
objects of those types. When Delectus encounters two objects with the
same UUID, it considers them two (possibly different) copies of the
same object. In that case, Delectus attempts to reconcile their
differences using the sync engine together with the change protocol.

**Columns** and **Rows** are assigned IDs that are guaranteed unique
only within a given List. A **Field** is uniquely identified within a
List by its row and column.

## Field values

Field values are the primitive, atomic value objects stored by a
Delectus container. They include the following types:

- **null:** The absence of any value
- **Boolean:** True or false
- **Number:** A numeric value
- **Text:** A text string
- **Link:** A URI reference to some resource
- **Reference:** A reference to a Delectus Data Object

## Rows

A **Delectus row** consists of a sequence of field values. Rows do not
occur bare; they exist only as members of **Delectus lists**.

## Columns

A **Delectus column** consists of a sequence of text values. Columns
do not occur bare; they exist only as attributes of **Delectus
lists**.

## Lists

A **Delectus list** consists of a sequence of **Delectus rows**,
together with a sequence of **Delectus columns**. In a given list, the
number of fields in each row must be equal to that of all the others,
and equal to the number of columns.

## Collections

A **Delectus collection** contains zero or more lists and
collections. Each list and each collection belongs to either zero or
one collection. Users may remove lists and collections from a
collection, and may add them to a collection.

## Containers

A **Delectus container** is a non-volatile resource that contains
other Delectus objects. Specifically, a container contains Lists and
collections.

Each container contains a single special collection called
"_Contents". The "_Contents" collection contains all other Delectus
objects, either directly or indirectly. An enumeration of "_Contents"
yields all of the containers and lists that are not contained in other
containers.

Users cannot delete "_Contents".

