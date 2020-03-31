# Delectus 2 Sync

## Protocol

The protocol for sync between nodes is described in sync-protocol.numbers.

## Reconciliation

The "process history" step in the protocol comprise the Delectus
**reconciliation** process. The steps in reconciliationare as follows:

1. Authenticate and initiate sync

The initiating node attempts to authenticate to its peer. If the
authentication fails then the sync as a whole fails without changing
any data in either peer.

If authentication succeeds, then the peer returns to the initiating
node a sync-initiated message with a temporary uuid. Both peers
temporarily record the sync-initiated message in local state, along
with a timeout value. If either peer fails to respond further within
the timeout then both peers treat the sync as failed and delete the
sync-initiated data without altering either one's log.

2. Find mismatch

Two datafiles with the dame contents have identical logs. Two with
different data have mismatches in their logs. The first step in
reconciliation is to compare logs, looking for the mismatches.

In general, Delectus nodes sync--that is, perform
reconciliations--many times. In a case where two nodes have synced
before, the parts of their logs that must be compared include only
those parts after the most recently-recorded sync. The logs prior to
that sync are already identical.

If no prior sync is found, or if the later parts of the reconciliation
fail, then the two nodes exchange their full histories for merging. If
that merge fails then the entire sync fails.

3. Sort and merge

Once the log sections to be merged are determined and exchanged, both
nodes perform the same operation: they merge the operation histories
and sort the result in revision-origin order, removing duplicates.

Revision-origin order means that the operations are first sorted by
revision, then, within identical revisions, by origin. Identical
revisions identify concurrent commits. Delectus arbitrarily chooses
which commit wins, giving the win to the origin UUID that sorts
higher. This outcome is arbitrary, but it's predictable and consistent
across nodes, since all participating nodes agree on each one's node.

Once reconciliation succeeds on both nodes, their commit histories are
identical.

4. Record the sync

Once the merge has finished on the initiating node, it sends a sync ok
message to its peer. If the peer succeeds in its merge, then it sends
back an acknowledgement with a sync ID. Both peers record a sync op in
their logs with the same ID.

If either peer fails, it sends a sync failed message to the other
peer. Both peers cancel the sync and delete the temporarily-stored
sync-initiated message without altering either log.

