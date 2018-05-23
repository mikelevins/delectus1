# Delectus 2 Changes Reference

Delectus records data by executing **change messages**. It stores the
data in a **storefile**. The storefile is a SQLite 3 file with a set
of tables that are described in "storefiles.md".

The canonical state of the Delectus data is stored in the "Changelog"
table. The state of the other remaining tables (except for the
"Delectus" table) can be reconstructed from the Changelog.

## Change operations

The only way for Delectus to make a change to its stored data is to
receive and process a **change message**. A **change message** is an
S-expression that identifies the name of a **change operation**,
together with parameter values that specify the details of the change
that is to be made.

Successful execution of a change operation follows these steps:

1. Read the change message

2. Identify the API function that implements the specified change
   operation

3. Apply the identified change function to the supplied parameter
   values

4. If the change function fails: signal a change error, and return a
   failure message to the requestor. In the case of failure, no change
   is made to the Delectus store, and the change message is not added
   to the log.

5. If the change function succeeds: return a success message to the
   requestor. Add the change message to the store's changelog. Compute
   a new state token from changelog--after adding the new message--and
   store it in the "state" column of the new message's row.

## The Changelog

The Changelog table records each successfully-executed change message
in order of execution. Executing each message in order against an
empty store faithfully reconstructs the state of the original store.

When the store is initially created, records a successful
:CREATE-STORE message in its changelog. It then computes an SHA3 hash
of the contents of the "message" column and stores the resulting
**state token** in the "state" column of that row.

For each subsequent change message, Delectus extracts a state token
parameter from the message and compares it to the "state" field of the
last row of the Changelog. If they are the same, then Delectus knows
that the change request was issued against a store that was in the
same state that the current store is presently in. Executing the
change will therefore bring the local store into congruence with the
originating store. (If the originating store is the local store then
the two state tokens are guaranteed to be the same, because the token
used in constructing the change message is the current value of the
last row's "state" field.)

If the state tokens differ then the two stores are out of
sync. Delectus initiates a synchronization of the two stores (see
"synchronization.md").

