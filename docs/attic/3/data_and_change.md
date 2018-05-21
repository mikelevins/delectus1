# Data and Change in Delectus 2.0

How Delectus stores data and tracks changes

## The Changelog

A Delectus object--a List or Collection--is uniquely and
authoritatively represented by its changelog. Given the changelog for
an object, the object itself can be reconstructed at any point in its
history. Starting with an empty object, we can reconstruct a reference
object at any point in its recorded history by applyiong its changes
to the empty object up to that point.

Delectus 2 uses SQLite files to store objects for convenience, but
everything other than the change log is best thought of as a cache
used to make interaction faster. The true representation of the object
is the changelog.

## Structure of the changelog

The only way to make a change to a Delectus object is to send a
**change message** to the node managing the object. Assuming the
message is valid, the node responds by updating the target object to
reflect the change requested by the message. It then adds the message
to the object's changelog, computes a digest of the updated changelog,
and stores the digest as the object's **state token**. Because a state
token is a digest of the object's history, state tokens from two
copies of an object can be compared to determine whether they are in
sync.

The changelog is represented as a list of change messages in reverse
temporal order. New messages are added to the head of the log.

