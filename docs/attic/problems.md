1. How do we represent destroyed lists and collections?

When a value, row, or column is removed from a list, we have a record
of what it was before it was deleted. We can back up to a state in
which the destroyed data are restored.

Likewise, when a List or collection is removed from another
collection, we have a record of the list or collection that used to be
there, and we can restore it, as long as the missing list or
collection still exists.

The problem is: what if it no longer exists?

For example, what if we destroy a list? Any references to it from a
collection are now references to a nonexistent list. How do we handle
it? Do we:

- automatically remove it from the collection once we notice that it
  no longer exists? If this one, do we notify the user that it has
  been destroyed?

- leave the reference in place, but render it in a special way in the
  UI to signify that the object can no longer be found?

When we sync, do we propagate the reference to the destroyed object? I
would think yes, but: what if it has not yet been destroyed on the
sync target? I begin to think that the change log needs to record
enough information to reconstruct destroyed objects (which basically
means storing a snapshot of the changelog of the destroyed object when
it's destroyed).

What if we destroy a collection or list that is not contained in any
other collection or list? If we record its changelog, where do we
record it, since it has no container? Perhaps we posit a container
abstraction for all collections and lists that a Delectus instance
knows about, and store change logs on it.

