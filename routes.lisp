;;;; routes.lisp

(in-package #:todolist)

(defun project-dir (&optional (dir ""))
  "Get full path to todolist elements"
  (asdf:system-relative-pathname "todolist" dir))

;;; Generate hunchentoot:define-easy-handler with json content-type
(defmacro json-router ((&key name uri (request-type :get)) &body body)
  `(hunchentoot:define-easy-handler (,name :uri ,uri :default-request-type ,request-type) ()
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
(json-router (:name api-group-list-handler :uri "/api/group/list")
  (api-group-list))

;;; Delete group
(json-router (:name api-group-delete-handler :uri "/api/group/delete" :request-type :post)
  (api-group-delete (hunchentoot:post-parameter "group")))

;;; Get list of todos
(json-router (:name api-todos-all-handler :uri "/api/todos/all")
  (api-todos-all))

;;; Get list of todos by group
(json-router (:name api-todos-by-group-handler :uri "/api/todos")
  (api-todos-by-group (hunchentoot:get-parameter "group")))

;;; Get todo by group and id
(json-router (:name api-todos-by-group-and-id-handler :uri "/api/todos/get")
  (api-todos-by-group-and-id (hunchentoot:get-parameter "group")
			     (hunchentoot:get-parameter "todo")))

;;; Change todo status
(json-router (:name api-todos-change-status-handler :uri "/api/todos/status/change" :request-type :post)
  (api-todos-change-status (hunchentoot:post-parameter "group")
			   (hunchentoot:post-parameter "todo")
			   (hunchentoot:post-parameter "status")))

;;; Change todo text
(json-router (:name api-todos-change-text-handler :uri "/api/todos/text/change" :request-type :post)
  (api-todos-change-text (hunchentoot:post-parameter "group")
			 (hunchentoot:post-parameter "todo")
			 (hunchentoot:post-parameter "text")))

;;; Delete todo by group and id
(json-router (:name api-todos-delete-handler :uri "/api/todos/delete")
  (api-todos-delete (hunchentoot:post-parameter "group")
		    (hunchentoot:post-parameter "todo")))

;;; Add new todo in group
(json-router (:name api-todos-add-handler :uri "/api/todos/add" :request-type :post)
  (api-todos-add (hunchentoot:post-parameter "group")
		 (hunchentoot:post-parameter "text")))

;;; Get todos statistics
(json-router (:name api-todos-get-stats-handler :uri "/api/todos/stats")
  (api-todos-get-stats))
