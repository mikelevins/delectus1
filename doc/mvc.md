# Delectus MVC

## Model, View, Controller

The Delectus **model** is the `store`, a thin wrapper class around
datafiles. The canonical file format in Delectus 2.x is SQLite 3. The
`store` class handles all direct interactions with SQLite, constructs
and executes SQL statements, manages metadata stored in the SQLite
file, and manages the undo queue.

The **view** depends on the delivery platform. For desktop systems,
including macOS, Linux, and Windows, the view class is designed in
terms of Lispworks CAPI. For iOS and Android the views are native
widgets communicating with the mocl runtime through its defcallback
mechanism. A web UI is planned, but its view architecture is TBD.

The **controller** is the `document`, a class that accepts and
processes commands that are enqueued by the views when user events
take place. The `document` class also manages the undo queue.

## Events, commands, undo

The view classes handle user events by constructing `command` objects
and posting them to the `document`.

When a `document` receives a `command` it **executes** it. Execution
means applying the `execute-command` generic function to the received
command object. `execute-command` performs any preflight operations,
then makes any required changes to the model, then posts notification
to the view of any changes that occurred.

For example, suppose a user edits a cell in a displayed document. The
view (that is, the UI) posts to the document a `command` that
represents the change required by the edit. The document calls
`execute-command`, which identifies the row that is to be changed by
the edit, records its current data in the `before` slot of the command
to be executed, constructs and executes the SQL needed to update the
row, adds the command object to the document's completed-commands log,
and posts a notification of the changed data to the view that
initiated the edit.

When a document's command log is not empty, the application's Undo
menu item becomes active. Selecting Undo retrieves the topmost command
from the log, constructs the SQL needed to change the data back to the
pre-command state, pushes the command onto the Redo log, and then
posts an update notification to the view.

Undo and Redo work the same way, except that Undo pops a command from
the Undo stack and pushes it on the Redo stack, and Redo does the
opposite.


