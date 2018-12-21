# Movies structure

## Account structure

```
{
  // User data
  email: string,
  fullName: string,
  // Delectus metadata
  username: string,
  CreatedTime: string,
  ModifiedTime: string,
  // CouchDB metadata
  _id: string, // CouchDB: org.couchdb.user:username
  _rev: string, // CouchDB
  type: "user",
  roles: array,
}
```

- **fullName:** The name by which the user wishes to be known.  

  This name is supplied by the user when creating an account. It must
  not represent an attempt to impersonate another user or well-known
  person. A fullName may contain whitespace, but leading and trailing
  whitespace are removed, and if two or more whitespace characters
  appear between other characters, then they are replaced by a single
  whitespace character.  

  For example, " Harlan Sanders " is converted to "Harlan
  Sanders". This coversion is called **canonicalization**. No two
  accounts may use the same canonicalized fullName.

- **username:** The unique id given to the user account when it's
  created.  
  
  The username is automatically derived from the fullName. After the
  fullName has been vetted, it is used to construct the username by
  lowercasing the text of the fullName and replacing whitespace
  characters with dots (periods).

## List structure

```
{
  // user data
  name: string, // may be null; freeform name field for the user's use
  notes: string, // may benull; freeform note field for the user's use
  // Delectus metadata
  type: "List",
  CreatedTime: string,
  ModifiedTime: string,
  // CouchDB metadata
  _id: string, // CouchDB
  _rev: string, // CouchDB
}
```

- **name:** The user-assigned name of the list.  

  This field may be null. If it's null, then the Delectus UI displays
  the first ListItem as the title of the list.

## Item structure

```
{
  // user data
  Title: string,
  Star: string,
  Costar: string,
  Subject: string,
  Number: number,
  Director: string,
  comments: string,
  Rating: string,
  Year: number
  // Delectus metadata
  type: "ListItem",
  CreatedTime: string,
  ModifiedTime: string
  // CouchDB metadata
  _id: string, // CouchDB
  _rev: string, // CouchDB
}
```

- **CreatedTime:** A Javascript Date string identifying the time the
  item was created.  
  
  The default collation order for lists is ascending by this field--in
  other words, in the same order that the items were
  added to the list.  

- **ModifiedTime:** A Javascript Date string identifying the most
  recent time the item was modified. 
