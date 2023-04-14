;;;; database.lisp

(in-package #:todolist)

(defparameter *db-file* (project-dir "./db"))
(defparameter *first-run* nil)

;;; Groups table
(mito:deftable groups ()
  ((name :col-type :text)))

;;; Statuses table
(mito:deftable statuses ()
  ((name :col-type :text)))

;;; Todos table
(mito:deftable todos ()
  ((group-id :col-type :integer)
   (status-id :col-type :integer)
   (text :col-type :text)))

(defun check-first-run ()
  "Check first run application"
  (when (not (probe-file *db-file*))
    (setf *first-run* t)))

(defun create-default-groups ()
  "Create default groups"
  (mito:create-dao 'groups :id 1 :name "work")
  (mito:create-dao 'groups :id 2 :name "study")
  (mito:create-dao 'groups :id 3 :name "finance")
  (mito:create-dao 'groups :id 4 :name "home")
  (mito:create-dao 'groups :id 5 :name "others"))

(defun create-default-statuses ()
  "Create default statuses"
  (mito:create-dao 'statuses :id 1 :name "TODO")
  (mito:create-dao 'statuses :id 2 :name "DOING")
  (mito:create-dao 'statuses :id 3 :name "DONE"))

(defun create-default-database-values ()
  "Create default database values, and create test note in todolist"
  (create-default-groups)
  (create-default-statuses)
  (mito:create-dao 'todos
		   :group-id 2
		   :status-id 1
		   :text "<p>Hello Friend! This is a test note, on it you can try how this application works.</p>"))

(defun database-connect ()
  "Connect to database"
  (format t "DATABASE: Starting connection...~%")
  (check-first-run)
  (mito:connect-toplevel :sqlite3 :database-name *db-file*)
  (when *first-run*
    (mapcar #'mito:ensure-table-exists '(todos groups statuses))
    (create-default-database-values)
    (setf *first-run* nil))
  (format t "DATABASE: Connection started...~%"))

(defun database-disconnect ()
  "Disconnect from database"
  (format t "DATABASE: Stopping connection...~%")
  (mito:disconnect-toplevel)
  (format t "DATABASE: Connection stopped...~%"))
