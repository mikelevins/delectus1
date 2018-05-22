# Delectus 2 Change Tracking

Delectus 2 tracks all changes made to each registered storefile (for
more about storefiles, see "stores.md"). Delectus defines a finite set
of change operations that may be made to a store. Each operation is
represented by a **change message**. Each change message that is
successfully executed is recorded in a **changelog** that is stored in
the storefile. The state of a storefile at any point in its history
can be reconstructed from the changelog. Two storefiles may be brought
into congruence by merging their changelogs and applying the merged
changes to each of them in history order.

This document describes the changelog and change messages.

## Change messages

Changes to the store are made by executing **change messages**. Each
change message corresponds to an operation that may be performed on a
storefile to change the state of its contents. The **changelog** is
the complete and authoritative record of all changes made to the
storefile. The state of other data structures in the storefile is
derived from the changelog.

The format of a change message is as follows:

```Lisp
(operation-name &rest parameters)
```

- **operation-name** is a Lisp keyword that names the operation to be
    performed

- **parameters** are values that serve as arguments to the change
    operation, providing all the data needed to execute or undo the
    requested change

## Change-operations reference

There are two elements that make up a **change operations**: a
**change message** and a corresponding **change-API function**.

A **change message** is an s-expression that gives the name of the
change operation and a sequence of parameter values. A **change-API
function** is a procedure that executes the change specified by the
message.

### Operations on stores

Each operation requires a store-id, which is the Delectus identity
string that uniquely identifies the affected store.

Each operation also requires a state token, which is a hash of the
pre-existing changelog. If the state token supplied in a message
differs from the state token recorded in the target store, then
Delectus rejects the change, returning a state-error value. In that
case, no change is made to the store or its changelog.

Operations that affect Lists or Collections also require the identity
string for the List or Collection object.

Operations that update a value or attribute require both the value
before the update and the value after the update. Delectus uses the
before value to ensure that the object state being updated is the
expected state, and it records the before value in the changelog to
enable undo.

If a change operation succeeds by updating the state of the store, it
returns the object directly affected by the change. The change message
is then added to the changelog. A new state token is computed from the
new changelog and is recorded as the store's current state. (The
changelog is also implicitly a log of states, because each recorded
change message contains the state token for the store's previous
state.)

- **create store**
    (:create-store store-id state-token)
    => store-id | store-creation-error

- **mark store deleted**
    (:mark-store-deleted store-id state-token old-status new-status)
    => store-id | store-deletion-error

- **update store owner**
    (:update-store-owner store-id state-token old-owner-id new-owner-id)
    => store-id | store-ownership-error

### Operations on Lists

- **create list**
    (:create-list store-id state-token list-id)
    => list-id | list-creation-error

- **mark list deleted**
    (:mark-list-deleted store-id state-token list-id old-status new-status)
    => list-id | list-deletion-error

- **update list owner**
    (:update-list-owner store-id state-token list-id old-owner-id new-owner-id)
    => list-id | list-ownership-error

- **update list name**
    (:update-list-name store-id state-token list-id old-name new-name)
    => list-id | list-name-update-error

- **update list note**
    (:update-list-note store-id state-token list-id old-note new-note)
    => list-id | list-note-update-error

- **add list column**
    (:add-list-column store-id state-token list-id column-label column-type)
    => list-id | list-column-add-error

- **mark list column deleted**
    (:mark-list-column-deleted store-id state-token list-id column-label old-status new-status)
    => list-id | list-column-deletion-error

- **update list column label**
    (:update-list-column-label store-id state-token list-id old-column-label new-column-label)
    => list-id | list-column-label-update-error

- **add list row**
    (:add-list-row store-id state-token list-id old-row-count new-row-count)
    => list-id | list-row-add-error

- **mark list row deleted**
    (:mark-list-row-deleted store-id state-token list-id row-id old-status new-status)
    => list-id | list-row-deletion-error

- **update field value**
    (:update-field-value store-id state-token list-id column-label row-id old-value new-value)
    => list-id | list-field-update-error

- **mark list shared**
    (:mark-list-shared store-id state-token list-id old-identity-list new-identity-list)
    => list-id | list-share-error

- **mark list public**
    (:mark-list-public store-id state-token list-id old-status new-status)
    => list-id | list-publish-error

### Operations on Collections

- **create collection**
    (:create-collection store-id state-token collection-id)
    => collection-id | collection-creation-error

- **mark collection deleted**
    (:mark-collection-deleted store-id state-token collection-id old-status new-status)
    => collection-id | collection-deletion-error

- **update collection owner**
    (:update-collection-owner store-id state-token collection-id old-owner-id new-owner-id)
    => collection-id | collection-ownership-error

- **update collection name**
    (:update-collection-name store-id state-token collection-id old-name new-name)
    => collection-id | collection-name-update-error

- **update collection note**
    (:update-collection-note store-id state-token collection-id old-note new-note)
    => collection-id | collection-note-update-error

- **add collection member**
    (:add-collection-member store-id state-token collection-id new-member-id)
    => collection-id | collection-member-addition-error

- **mark collection member deleted**
    (:mark-collection-member-deleted store-id state-token collection-id member-id old-status new-status)
    => collection-id | collection-member-deletion-error

- **mark collection shared**
    (:mark-collection-shared store-id state-token collection-id old-identity-list new-identity-list)
    => collection-id | collection-share-error

- **mark collection public**
    (:mark-collection-public store-id state-token collection-id old-status new-status)
    => collection-id | collection-publish-error



