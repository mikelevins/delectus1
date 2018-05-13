# Delectus 2 sync

The Delectus **data engine** stores **delectus lists**. The Delectus
**sync engine** enables two different copies of a Delectus list to be
synchronized, so that differences in their stored data can be
reconciled. A network of Delectus instances storing the same list are
**eventually consistent**: although local changes may cause different
copies to temporarily contain different data, communication between
Delectus instances eventually brings the different copies back into
agreement.

The program that reconciles different copies is the **sync engine**.

## What sync does

When two Delectus instances communicate, they compare
identifiers. First, they determine whether they are owned by the same
account. If the account identifiers differ (or if one or both are
absent), then the sync engine does not attempt to synchronize, and the
communication ends.

When the account identifiers are the same, the two engines compare
list identifiers. If the set of list identifiers for one engine is
congruent to the set for the other, then the two sets are in synch,
and the engines are done.

If the two sets of identifiers are not congruent, then there is a
difference in the data that each engine has. The sync engines initiate
an exchange of deltas to reconcile them.

The identifier of a list consists of an **identity token**, which
distinguishes it from all other Delectus lists, a **change history**,
and a **state history**.

The **change history** is a log of **change operations** that have
been applied to the list locally. The current state of a list is the
cumulative result of a series of change operations.

The **state history** is a log of hashes. After each change operation,
the sync engine computes a hash of the list's current state and
appends it to the state history. Each **state token** corresponds to a
specific **change operation** that produced it.

When two sync engines compare identifiers, they check to see if their
current state tokens are equal. If they aren't, then the engines know
that their data differs, which means that their change histories
differ. They can reconcile the differences by merging change histories
and replaying the change operations from the point at which they
diverged.

So: when two engine discover they have different state tokens for the
same list identifier, they exchange state histories to find the point
in the histories where the state tokens become different. They then
exhange change histories from that point forward, merging them in
order, so that both engines have the same change histories. Each
engine then rolls back its change history to the beginning of thw
newly-merged change history, then applies the merged changes in order,
computing new a new state token for each change. When they're done
applying changes, the two engines are synchronized and contain the
same data. They're ready for the next sync test.

## Requirements on data storage

Each sync engine has to store the information needed to identify
differences between its local state and the state of any remote that
it syncs with.

Each sync engine's state is managed by its associated data engine. The
responsibility of the data engine is to create new lists, respond to
change operations by altering the data stored in them, and maintain
the data used by the sync engine: the list identifier, the change
history, and the state history.

In order to maintain the integrity of the sync system, the data engine
must alter the contents of its store only in response to valid change
operations, and it must always compute a hash of the resulting data
state and store it alongside the corresponding change operation.

When the sync engine orders a reconciliation, the data engine must
undo the changes identified by the sync engine for reversal, then redo
the new changes supplied by the sync engine.

