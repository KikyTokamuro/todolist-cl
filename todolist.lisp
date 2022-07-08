;;;; todolist.lisp

(in-package #:todolist)

(defvar *server-connection*)

(defun start-server (&optional (port 8080))
  "Start the Hunchentoot connection on PORT."
  (format t "SERVER: Starting connection...~%")
  (database-connect)
  (setf *server-connection* (hunchentoot:start
		      (make-instance 'hunchentoot:easy-acceptor
				     :port port)))
  (format t "SERVER: Connection started~%"))

(defun stop-server ()
  "Kill the Hunchentoot connection."
  (format t "SERVER: Stopping connection...~%")
  (hunchentoot:stop *server-connection*)
  (setf *server-connection* nil)
  (database-disconnect)
  (format t "SERVER: Connection stopped~%"))

(defun restart-server (&optional (port 8080))
  "Restart the Hunchentoot connection on PORT."
  (format t "SERVER: Restarting connection~%")
  (when *server-connection*
    (stop-server))
  (start-server port)
  (format t "SERVER: Connection restarted~%"))
