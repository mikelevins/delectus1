
(in-package #:delectus)

;;; some tools for generating data for SQLite
;;; the idea is to generate enough data to simulate an op log from two
;;; different nodes, write it out as CSV, import it into a SQLite
;;; file, then do some SQL queries to see if I can get the desired
;;; results

;;; nodes
(defparameter $nodeid1 "IDa15870d5_cc96_4031_b33b_f83d382b455a")
(defparameter $nodeid2 "IDea210fe7_9c0e_40a9_9e11_cf22a1248a33")
(defparameter $revision-counter 0)
(defparameter $item-counter 0.0)

;;; ops
(defparameter $listname "listname")
(defparameter $columns "columns")
(defparameter $item "item")
(defparameter $sync "sync")

;;; columns
(defparameter $colid0 "ID57a71a3d_9704_456c_aeeb_50b17c4658c2")
(defparameter $col0 (make-column "Item ID" :id $colid0 :deleted nil :order 10.0 :type "text" :sort "asc"))
(defparameter $colid1 "IDe37b7018_35d2_43c4_8159_e09c3ac9bb9c")
(defparameter $col1-1 (make-column "node id" :id $colid1 :deleted nil :order 20.0 :type "text" :sort nil))
(defparameter $col1-2 (make-column "NODE ID" :id $colid1 :deleted t :order 20.0 :type "text" :sort nil))
(defparameter $colid2 "ID8c2bf9bb_5baa_4848_9967_70800561ccea")
(defparameter $col2 (make-column "Word" :id $colid2 :deleted nil :order 30.0 :type "text" :sort nil))
(defparameter $columns1 (list $col0 $col1-1 $col2))
(defparameter $columns2 (list $col0 $col1-2 $col2))

;;; words
;;; we'll build a list of words randomly chosen from /usr/share/dict
(defparameter $words
  (let ((arr (make-array 235886)))
    (with-open-file (in "/usr/share/dict/web2")
      (loop for word = (read-line in nil nil nil) then (read-line in nil nil nil)
         counting word into i
         while word do (setf (elt arr (1- i)) word)))
    arr))

;;; (length $words)
;;; (elt $words 0)
;;; (elt $words 235885)

(defun op (type opid origin revision itemid itemorder deleted name columns)
  (append (list type opid origin revision itemid itemorder deleted name)
          columns))

(defun fields (f0 f1 f2)
  (list f0 f1 f2))

;;; (op $columns (new-identifier) $nodeid1 (incf $revision-counter) nil nil nil $columns)
;;; (op $item (new-identifier) $nodeid1 (incf $revision-counter) (new-identifier) nil nil (fields 0 1 2))

(defparameter $test-csv-file "/Users/mikel/Workshop/data/delectus/delectus-test.csv")

(defun newid ()(new-identifier))
(defun increv ()(incf $revision-counter))
(defun incitem ()(incf $item-counter 10.0))

(defun to-json-object (plist)
  (let ((plist (loop for tail on plist by #'cddr
                  append (list (intern (first tail) :keyword)
                               (second tail)))))
    (jonathan:to-json plist)))

;;; (to-json-object $col0)

(defun get-colid (col)(second col))

(defun write-test-csv (path)
  (with-open-file (out path :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((cols1 (mapcar #'to-json-object $columns1))
          (cols2 (mapcar #'to-json-object $columns2)))
      ;; write the listname and columns ops
      (let* ((type $listname)
             (opid (newid))
             (origin $nodeid1)
             (revision (increv))
             (itemid nil)
             (itemorder nil)
             (deleted nil)
             (name nil)
             (columns nil)
             (namefields (op type opid origin revision itemid itemorder deleted name columns)))
        (fare-csv:write-csv-line namefields out))
      ;; write items and columns updates
      (loop for i from 0 below (length $words)
         do (let ((word (elt $words i)))
              ;; for multiples of 7, update the columns
              (when (zerop (mod i 7))
                (let* ((type $columns)
                       (opid (newid))
                       (origin (if (evenp i) $nodeid1 $nodeid2))
                       (revision (increv))
                       (itemid nil)
                       (itemorder nil)
                       (deleted nil)
                       (name nil)
                       (columns (if (evenp i) cols1 cols2))
                       (fields (op type opid origin revision itemid itemorder deleted name columns)))
                  (fare-csv:write-csv-line fields out)))
              ;; write the item op
              ;; first, write the node1 version
              (let* ((type $item)
                     (opid (newid))
                     (origin $nodeid1)
                     (revision (increv))
                     (itemid (new-identifier))
                     (itemorder (float i))
                     (deleted nil)
                     (name nil)
                     (vals (list itemid $nodeid1 word))
                     (fields (op type opid origin revision itemid itemorder deleted name vals)))
                (fare-csv:write-csv-line fields out)
                ;; if i is odd, now write the node2 version
                (when (oddp i)
                  (let* ((origin $nodeid2)
                         (vals (list itemid $nodeid2 (string-upcase word)))
                         (fields (op type opid origin revision itemid itemorder deleted name vals)))
                    (fare-csv:write-csv-line fields out)))))
           'done))))

;;; (time (write-test-csv $test-csv-file))

;;; Notes
#|

SQL to create the schema:

create table DelectusTest (
  `type`, opid, origin, revision INT, 
   itemid, itemorder DOUBLE, deleted, name, 
   `ID57a71a3d_9704_456c_aeeb_50b17c4658c2` text, 
   `IDe37b7018_35d2_43c4_8159_e09c3ac9bb9c` text, 
   `ID8c2bf9bb_5baa_4848_9967_70800561ccea` text);


SQL that returns all items sorted by revision, then origin:

select `ID8c2bf9bb_5baa_4848_9967_70800561ccea`, revision, origin 
from DelectusTest 
where `type`="item" 
order by revision,origin asc;


This query returns the correct values; that is, when there are two
values with the same revision number, it returns the one with the
higher-sorted origin. 

SELECT a.itemorder,a.`ID8c2bf9bb_5baa_4848_9967_70800561ccea`, a.revision, a.origin
  FROM (SELECT itemid, itemorder, revision, `ID8c2bf9bb_5baa_4848_9967_70800561ccea`, origin,
               ROW_NUMBER() OVER (PARTITION BY itemid ORDER BY revision, origin DESC) rank
          FROM DelectusTest WHERE type="item") a
 WHERE a.rank = 1 order by a.itemorder;

Timings:
- 7.5 sec on 387528 rows
- 2.1 sec on 100000 rows
- 1.1 sec on 50000 rows

|#
