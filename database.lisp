;;;; database.lisp

(in-package #:todolist)

(defparameter *db-file* (project-dir "./db"))
(defparameter *first-run* nil)

;;; Todos table
(mito:deftable todos ()
	       ((group :col-type :text)
		(text :col-type :text)
		(status :col-type :text)))

;;; Groups table
(mito:deftable groups ()
	       ((name :col-type :text)))

(defun check-first-run ()
  "Check first run application"
  (when (not (probe-file *db-file*))
    (setf *first-run* t)))

(defun create-default-database-values ()
  "Create default database values, and create test note in todolist"
  (mito:create-dao 'groups :name "work")
  (mito:create-dao 'groups :name "study")
  (mito:create-dao 'groups :name "finance")
  (mito:create-dao 'groups :name "home")
  (mito:create-dao 'groups :name "others")
  (mito:create-dao 'todos
		   :group "study"
		   :text "Hello Friend! This is a test note, on it you can try how this application works."
		   :status "todo"))

(defun database-connect ()
  "Connect to database"
  (format t "DATABASE: Starting connection...~%")
  (check-first-run)
  (mito:connect-toplevel :sqlite3 :database-name "db")
  (mapcar #'mito:ensure-table-exists '(todos groups))
  (when *first-run*
    (create-default-database-values)
    (setf *first-run* nil))
  (format t "DATABASE: Connection started...~%"))

(defun database-disconnect ()
  "Disconnect from database"
  (format t "DATABASE: Stopping connection...~%")
  (mito:disconnect-toplevel)
  (format t "DATABASE: Connection stopped...~%"))
