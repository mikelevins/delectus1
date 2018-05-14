# Delectus 2 Data Objects

## Field values

- **null:** The absence of any value
- **Boolean:** True or false
- **Number:** A numeric value
- **Text:** A text string
- **Link:** A URI reference to some resource
- **Reference:** A reference to a Delectus Data Object

## Rows

A **Delectus row** consists of a sequence of field values. Rows do not
appear bare; they exist only as members of **Delectus lists**.

## Columns

A **Delectus column** consists of a sequence of text values. Columns
do not appear bare; they exist only as attributes of **Delectus
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

