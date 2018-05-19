;;; changes.lisp
;;; logging changes

(in-package :engine)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Lists and collections maintain change logs. A change log records
;;; all of the changes to the object, expressed as change messages. A
;;; change message identifies the object to be changed, the attribute
;;; of the object to be changed, the old value of the attribute, the
;;; new value of the attribute, and the state of the target's change
;;; log before the change is executed.
;;;
;;; The state of the changelog is represented as a hash of the change
;;; log before the new change is made.
;;;
;;; The format of the hcange log is a list of change records, with new
;;; records added to the front of the list.
;;;
;;; A change record has the form:
;;;
;;; (changeindex previous-state change-message)
;;;
;;; where:
;;;
;;; changeindex is a monotonically-increasing integer that begins at
;;; zero when the object is created, and is incremeneted by 1 each
;;; time a change is completed
;;;
;;; previous-state is a hash of the entire changelog as it existed
;;; before the execution of the last recorded change message.
;;;
;;; A change-message is a list representing a request for one of the
;;; change operations that delectus defines on objects. A change
;;; message has the form:
;;;
;;; (operation-name . arguments)
;;;
;;; The operation name is a keyword that identifies the requested
;;; operation. The arguments are the values required as parameters to
;;; the requested change operation.
;;;
;;; After a node successfully executes a change message, it pushes a
;;; new change record onto the change log with the contents
;;;
;;; (changeindex+1 new-state new-change-message)
;;;
;;; where new-state is the hash of the changelog before the new
;;; message was executed.
;;;
;;; When two nodes communicate, they compare lists of the objects they
;;; are tracking. When one has an object that the other lacks, the one
;;; that has it packages the new object's changelog and ships it to
;;; the one that lacks it. The receiving node reconstructs the object
;;; in its store.
;;;
;;; When two nodes have the same object (as identified by comparing
;;; their identity tokens), they compare changeindexes and
;;; previous-states. If either of these values differs between them,
;;; then they know that their changelogs are out of sync. Each node
;;; packages the appropriate changelog and ships it to the other. Each
;;; node merges its own copy of the changelog with the received one,
;;; then empties the corresponding object, then applies the merged
;;; changes. When this process is complete, the two objects are in
;;; sync.


