# roles

## nobody

The `nobody` role is granted to unauthenticated visitors. It grants
the fewest permissions of all roles:

- read published lists
- comment on published lists for which anonymous comments are enabled

## user

Granted to authenticated users.

- read published lists
- comment on published lists for which comments are enabled
- create,edit, and delete own lists
- publish and unpublish own lists
- enable and disable anonymous and authenticated comments on own lists

## delectus

A special user, `delectus` has permissions identical to `user`, but
the lists that `delectus` publishes become the contents of the public
Delectus website.

## admin

- perform all the tasks a `user` can perform, but on other users' lists
- create, modify, enable, and disable user accounts and permissions

## ops

- stop and start the delectus service
- initiate backups and restores
- update the delectus software to a new release
- control monitoring and analytics tools

