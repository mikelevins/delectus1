# Delectus 2 Storage Design

Delectus nodes are responsible for storing users' lists and
collections locally, for backing them up on a Delectus server, and for
ensureing that a user's other devices have access to them. This
document explains how Delectus' local and network storage are
organized to make all this possible.

## Identity

Delectus deals with several types of objects that can exist in
multiple locations, and that must be kept in sync. Different copies
may be updated in different ways at different times. Delectus needs a
way to determine that two dissimilar objects are actually different
versions of the same object. It accomplishes that goal using
**identities**.

An **identity** is a unique identifier that is stored on a copy of an
object. If two objects have the same identifier, then they are treated
as different versions of the same object. Delectus uses its sync
protocol to modify both until their states are the same.

Objects with identity include:

- **Lists:** the basic unit of Delectus user data

- **Collections:** groups of lists that users use to organize lists

- **User accounts:** objects that represent persons who own Delectus
    data

- **Containers:** objects that store Delectus data persistently

- **Repositories:** objects that Delectus nodes use to keep track of
    the other objects that they store

## Repositories and containers

Each Delectus Node manages one or more **containers**. A container is
persistent storage for Delectus data. A node may manage a single
container, or it may manage several, with different objects contained
in different containers. Generally speaking, a node creates more than
one container if its storage requirements stress the capacities of a
single container. Examples would be Delectus servers, which manage
data for many users, or a client node that manages a large volume of
data for a single user.

In order to keep track of which objects are stored in which container,
and of which user account owns each object, the node also maintains an
object called a **repository**. The repository is a record of the
identities stored in each container.

## Container formats

In Delectus 2, containers and repositories are represented by SQLite
files. 
