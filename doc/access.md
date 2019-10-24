# Delectus Ownserhip and Access

## User Accounts

Each Delectus artifact has an **owner**. The owner is a **user
account** controlled by a **registered user**. Each registered user is
a person who has established as account that Delectus can
authenticate, and which exercises CRUD powers over the Delectus
artifacts owned by the account.

Delectus also maintinas some special administrative accounts with
plenary powers over all managed content and ordinary user accounts.

A user account stores contact and billing information, along with a
unique identifier and authentication data. Authentication and
authorization create sessions that the user can use to create,
retrieve, update, and delete Delectus artifacts. All artifacts created
in an authenticated session are owned by the account that created
them. No other account may alter those artifacts or read them without
permission of the owner, except for the special administrative
accounts maintained by Delectus.

The owner of an artifact may optionally share it for reading or
editing with selected other users, or may publish it, which makes it
readable to all users of Delectus public channels. An owner may also
selectively enable the general public to edit individual lists and
collections.

## Artifacts

The artifacts that users can create and control include:

- **Lists**: named documents consisting of **values** in **rows** and
  **columns**. **Values** may be numbers, text strings (including
  timestamps and links to URLS), or Boolean values. Each entry in a
  List is a **row** of values. Each row contains a number of values
  equal to the number of the List's **columns**. Each **column** has a
  text label, unique within the List.
  
- **Collections**: named documents that consist of lists of
  Lists. Each List is in at least one Collection (at a minium, in the
  owner's Default collection).

## Access

**Access** refers to a set of permissions governing what operations
each user may perform on a Delectus artifact. Access to an artifact is
granted to a user by the artifact's owner.

### Access rights

Each artifact is governed by the following **access rights**:

- Lists
  - Create and Delete
  - Insert and Remove
  - Rename
  - Update
  - Read

- Collections
  - Create and Delete
  - Insert and Remove
  - Rename
  - Share
  - Read

### Agents

There are three categories of **agents**:

1. **Owners** always have all access rights over artifacts that they own.

2. Members of a **Group** have the access rights over an artifact that
   the owner has granted to the group. Owners may create and name
   groups, and may add or remove other users at any time. Owners may
   grant to or revoke from a group the following access rights over an
   artifact:
   
  - Lists
    - Insert and Remove
    - Rename
    - Update
    - Read

  - Collections
    - Insert and Remove
    - Rename
    - Share
    - Read

3. Members of the **Public** include everyone who is able to view
   content through the Delectus web presence. The Public is a special
   permanent group owned by Delectus, and including everyone who has
   the ability to view Delectus content. The owner of an artifact can
   grant and revoke the same rights for the Public as for any group
   that they own.


