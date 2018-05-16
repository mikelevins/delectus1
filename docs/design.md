# The Design of Delectus 2

Delectus 2 is a distributed application that manages **lists** and
**collections** of lists.

## Delectus from the user's point of view

### Data objects

Using Delectus, users create, view, and edit **lists** and
**collections** of lists.

A **list** is a named object that contains zero or more **rows**. Each
**row** contains zero or more **fields** in named **columns**. Each
**field** contains exactly one **value**. All of the rows in a given
list have the same columns.

A **collection** is a named object that contains zero or more lists
and other collections.

### Types of values

The **values** that can be stored in fields belong to the following
types:

| Type      | Description                  |
| --------- | ---------------------------- |
| Null      | No value                     |
| Boolean   | True or False                |
| Number    | A numeric quantity           |
| Text      | A text string                |
| Timestamp | An ISO 8601 time string      |
| Link      | A URL                        |
| Reference | A reference to a Data Object |

### Copies of lists and collections

Delectus automatically backs up a user's lists to network
storage. Users can edit their lists and collections on any device
running Delectus, and the application automatically keeps track of all
the changes, periodically updating all the copies to keep them in
sync.

Each copy of a user's lists and collections keeps a complete change
history, enabling a user to undo every change all the way back to when
the object was first created, or to recover any data that has been
deleted.

## Delectus as a distributed application

Delectus is made up of parts called **nodes**. A Delectus **node** is
a process running on a device that manages a set of Delectus lists and
collections, and provides services to users or other nodes.

There are three types of nodes:

1. The Delectus **portal** presents a web page that publishes
Delectus-focused content, including selected lists and containers that
users have chosen to publish. It also servers as an administrative
interface for Delectus users. Users sign up and manage their accounts
and services using the portal.

2. Delectus **clients** run on user devices and provide the standard
Delectus user interface. When an individual user runs a Delectus
client, the user has access to all of their lists and collections, but
not to anyone else's. A client is a user's primary point of contact
with Delectus. Most users will think of the client as being Delectus.

3. The Delectus **server** accepts connections from Delectus client
nodes and creates and manages backups of their data. The server does
the same job as a client, in that it responds to requests to fetch and
update lists and collections; the difference from a client is that the
server manages lists and collections for all Delectus users, rather
than just one at a time. As you would expect, the server's user
interface is different, because of the server's different role. It's
primarily concerned with providing a management interface that
administrators can use to help users make the best use of their
accounts and data.

## How Delectus stores data

Besides **lists** and **collections**, the data objects that users are
concerned with, Delectus manages several other objects that it uses to
make lists and collections work as users expect.

### User accounts

A **user account** is an object that represents a person who can
create and manipulate lists and collections in Delectus. Every
Delectus list and collection is owned by a user account, and the
person who owns the user account owns and controls the data.

A new user must create a user account before using Delectus to create
and manipulate lists and collections. Delectus immediately registers
the new user account with the server and arranges to copy the
account's data to the server and automatically keep it in sync with the
user's device. The user may install Delectus on any number of personal
devices; Delectus automatically synchronizes all Delectus data owned
by the same account on any number of devices.

Ordinary Delectus users can create, view, and edit lists and
collections that they own. They can also share lists and collections
with other users, or publish them to user pages on the Delectus
website.

Specialized user accounts grant additional powers. For example, admin
users can create user accounts, disable and enable them, and edit
account information and user data in order to help users who get into
jams. The details of specialized accounts are not yet worked out.

### Containers

A **container** is a unit of storage used by a Delectus node to hold
lists and collections. A container keeps track of the objects stored
in it, including a history of objects added to and removed from it.

A client node normally manages a single collection, all of whose
contents belong to a single user account. A server node normally
manages content belonging to many user accounts, and the content may
be spread over many containers.

### Messages

Delectus nodes communicate with each other and synchronize data by
exchanging **messages**. Delectus defines a set of messages with a
defined format that nodes use to coordinate their activity. These
messages are organized into three protocols:

| Protocol | Description |
| -------- | ----------- |
| view | requests contents from a node's container for display and interaction |
| change | requests updates to stored data |
| sync | checks for and initiates attempts to synchronize two nodes' data |

Server nodes may define additional protocols to coordinate management
of server resources.

### Logs

**Logs** are data structures that record changes to the state of a
node. Delectus 2 uses two kinds of logs:

| Log type | Description |
| -------- | ----------- |
| change | a record of all changes to a list, collection, or container | 
| state | a cryptographic digest of a change log |

## How Delectus makes, tracks, and syncs changes

Each Delectus node keeps a log of all change-protocol messages that it
processes; this is the **change log**.

The only way to make a change to node data is to send the node a
change message for processing. Because each node records a complete
log of all change messages processed for each data object, it's always
possible to produce an exact copy of a list or collection by starting
with an empty list or collection and then applying the messages from
the original's change log. This process is how Delectus nodes sync
copies of lists and collections.

Delectus nodes use state logs to recognize when two copies of an
object are out of sync. If two objects are in sync, then their change
logs are congruent. That means that a cryptographic digest of one
object's change log will be the same as a cryptographic digest of the
other's. If the two object's digests are different, then Delectus
knows they are out of sync, and a synchronization is needed.

Each node keeps a log of states to make it easier to coordinate
synchronization. By comparing the state logs of two objects, Delectus
can identify the point in history where the two objects
diverged. Reconciling the two objects then consists of:

1. merging the parts of the change logs that differ
2. rewinding both objects to the last state at which they were the same
3. replaying the merged change log to bring both objects up-to-date

What if there have been conflicting changes to the two objects?

In that case, Delectus sorts the conflicting change messages
deterministically and applies them both, one after the other, so that
all edits made to any connected nodes are always recorded. That way,
even if one edit overwrites another, the overwritten edit it not lost;
it remains in the change history, and the client interface offers
users a way to recover changes that have been overwritten.

> Note: The deterministic method of sorting change messages is not yet
implemented. The chief requirement is that it must produce the same
order on all Delectus implementations.

## Sharing and publishing lists and collections

Delectus users have the option of **sharing** their lists and
collections with other Delectus users, or of **publishing** them on
the Delectus portal.

**Sharing** lists and collections means making them available for
other Delectus users to view and, optionally, to edit.

**Publishing** them means making them appear publicly on a user's
public web page on the Delectus portal.

The details of how sharing and publishing work are still to be worked
out.


