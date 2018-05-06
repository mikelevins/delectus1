# Delectus Data Design 2.0

## Delectus Objects

- **Database**
  A user-specific container for lists and collections. Each user has
  their own.

- **List**
  A named set of **Items** assembled by a **User**.
  Fields:
    "_id": a string uniquely identifying this specific list
    "type": always the string "List"
    "name": A string name
    "note": A string description
    "delectus:Viewers: 

- **Item**
  A set of **Values** colected together in a **List**.

- **Value**
  A String, Number, or Boolean, or an object of type Collection,
  `List`, `User`, `Link`, or `Time`.

- **Collection**
  A named set of Lists or Collections.

- **User**
  An account belonging to a person registered as a user of Delectus.

- **SpecialList**
  A system data structure that presents a List-like interface for the
  user's convenience.

- **SpecialCollection**
  A system data structure that presents a Collection-like interface for the
  user's convenience.

- **SpecialUser**
  A system data structure that presents a User-like interface for the
  user's convenience.



