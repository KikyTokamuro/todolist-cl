;;;; routes.lisp

(in-package #:todolist)

(defun project-dir (&optional (dir ""))
  "Get full path to todolist elements"
  (asdf:system-relative-pathname "todolist" dir))

;;; Generate hunchentoot:define-easy-handler with json content-type
(defmacro json-router (name uri &rest body)
  `(hunchentoot:define-easy-handler (,name :uri ,uri) ()
    (setf (hunchentoot:content-type*) "application/json")
    ,@body))

;;; Static files handler
(push (hunchentoot:create-folder-dispatcher-and-handler "/static/" (project-dir "./static/"))
      hunchentoot:*dispatch-table*)

;;; Index page
(hunchentoot:define-easy-handler (home-page-handler :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (index-page))

;;; Get csv with todos
(hunchentoot:define-easy-handler (api-todos-export-csv-handler :uri "/api/generate/csv") ()
  (setf (hunchentoot:content-type*) "text/csv")
  (setf (hunchentoot:header-out "Content-Disposition" )
	(format nil "inline; filename=todolist-cl ~A.csv"
		(local-time:format-timestring nil (local-time:now)
					      :format local-time:+asctime-format+)))
  (api-todos-export-csv))


;;; Get list of groups
(json-router api-group-list-handler "/api/group/list"
	     (api-group-list))

;;; Delete group
(json-router api-group-delete-handler "/api/group/delete"
	     (api-group-delete (hunchentoot:get-parameter "group")))

;;; Get list of todos
(json-router api-todos-all-handler "/api/todos/all"
	     (api-todos-all))

;;; Get list of todos by group
(json-router api-todos-by-group-handler "/api/todos"
	     (api-todos-by-group (hunchentoot:get-parameter "group")))

;;; Get todo by group and id
(json-router api-todos-by-group-and-id-handler "/api/todos/get"
	     (api-todos-by-group-and-id (hunchentoot:get-parameter "group")
					(hunchentoot:get-parameter "todoid")))

;;; Change todo status
(json-router api-todos-change-status-handler "/api/todos/status/change"
	     (api-todos-change-status (hunchentoot:get-parameter "group")
				      (hunchentoot:get-parameter "todoid")
				      (hunchentoot:get-parameter "status")))

;;; Change todo status
(json-router api-todos-change-text-handler "/api/todos/text/change"
	     (api-todos-change-text (hunchentoot:get-parameter "group")
				    (hunchentoot:get-parameter "todoid")
				    (hunchentoot:get-parameter "text")))

;;; Delete todo by group and id
(json-router api-todos-delete-handler "/api/todos/delete"
	     (api-todos-delete (hunchentoot:get-parameter "group")
			       (hunchentoot:get-parameter "todoid")))

;;; Add new todo in group
(json-router api-todos-add-handler "/api/todos/add"
	     (api-todos-add (hunchentoot:get-parameter "group")
			    (hunchentoot:get-parameter "text")))

;;; Get todos statistics
(json-router api-todos-get-stats-handler "/api/todos/stats"
	     (api-todos-get-stats))
