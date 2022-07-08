;;;; routes.lisp

(in-package #:todolist)

(defun project-dir (&optional (dir ""))
  "Get full path to todolist elements"
  (asdf:system-relative-pathname "todolist" dir))

;;; Static files handler
(push (hunchentoot:create-folder-dispatcher-and-handler "/static/" (project-dir "./static/"))
      hunchentoot:*dispatch-table*)

(hunchentoot:define-easy-handler (home-page-handler :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (index-page))

(hunchentoot:define-easy-handler (api-group-list-handler :uri "/api/group/list") ()
  (setf (hunchentoot:content-type*) "application/json")
  (api-group-list))

(hunchentoot:define-easy-handler (api-group-delete-handler :uri "/api/group/delete") ()
  (setf (hunchentoot:content-type*) "application/json")
  (api-group-delete (hunchentoot:get-parameter "group")))

(hunchentoot:define-easy-handler (api-todos-all-handler :uri "/api/todos/all") ()
  (setf (hunchentoot:content-type*) "application/json")
  (api-todos-all))

(hunchentoot:define-easy-handler (api-todos-by-group-handler :uri "/api/todos") ()
  (setf (hunchentoot:content-type*) "application/json")
  (api-todos-by-group (hunchentoot:get-parameter "group")))

(hunchentoot:define-easy-handler (api-todos-change-status-handler :uri "/api/todos/status/change") ()
  (setf (hunchentoot:content-type*) "application/json")
  (api-todos-change-status (hunchentoot:get-parameter "group")
			   (hunchentoot:get-parameter "todoid")
			   (hunchentoot:get-parameter "status")))

(hunchentoot:define-easy-handler (api-todos-delete-handler :uri "/api/todos/delete") ()
  (setf (hunchentoot:content-type*) "application/json")
  (api-todos-delete (hunchentoot:get-parameter "group")
		    (hunchentoot:get-parameter "todoid")))

(hunchentoot:define-easy-handler (api-todos-add-handler :uri "/api/todos/add") ()
  (setf (hunchentoot:content-type*) "application/json")
  (api-todos-add (hunchentoot:get-parameter "group")
		 (hunchentoot:get-parameter "text")))

(hunchentoot:define-easy-handler (api-todos-export-csv-handler :uri "/api/generate/csv") ()
  (setf (hunchentoot:content-type*) "text/csv")
  (setf (hunchentoot:header-out "Content-Disposition" )
	(format nil "inline; filename=todolist-cl ~A.csv"
		(local-time:format-timestring nil (local-time:now)
					      :format local-time:+asctime-format+)))
  (api-todos-export-csv))
