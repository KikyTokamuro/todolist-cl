;;;; api.lisp

(in-package #:todolist)

(defun json-error (message)
  "Generate JSON error"
  (jonathan:to-json (list :error message)))

(defun todo-to-list (todo)
  "Todo -> list"
  (let ((status (mito:find-dao 'statuses :id (todos-status-id todo)))
	(group (mito:find-dao 'groups :id (todos-group-id todo))))
    (list :id (mito:object-id todo)
	  :group-id (mito:object-id group)
	  :group-name (groups-name group)
	  :status-name (statuses-name status)
	  :status-id (mito:object-id status)
	  :text (todos-text todo)
	  :date (local-time:format-timestring nil (mito:object-updated-at todo)
					      :format local-time:+asctime-format+))))

(defun api-group-list ()
  "Return list of groups"
  (jonathan:to-json (mapcar #'(lambda (group)
				(list :id (mito:object-id group) :name (groups-name group)))
			    (mito:select-dao 'groups))))

(defun api-group-delete (group-id)
  "Delete group"
  (mito:delete-by-values 'groups :id group-id)
  (mito:delete-by-values 'todos :group-id group-id)
  (jonathan:to-json '(:group group-name)))

(defun api-todos-all ()
  "Return all todos"
  (jonathan:to-json
   (loop for todo in (mito:select-dao 'todos)
	 collect (todo-to-list todo))))

(defun api-todos-by-group (group-id)
  "Return todos from group"
  (jonathan:to-json
   (loop for todo in (mito:retrieve-dao 'todos :group-id group-id)
	 collect (todo-to-list todo))))

(defun api-todos-by-group-and-id (group-id todo-id)
  "Return todo by group and id"
  (cond ((null (and group-id todo-id))
	 (json-error "Empty params"))
	(t
	 (let ((todo (mito:find-dao 'todos :id todo-id :group-id group-id)))
	   (if (not todo)
	       (json-error "Todo not found")
	       (progn
		 (jonathan:to-json (todo-to-list todo))))))))

(defun api-todos-change-text (group-id todo-id text)
  "Change todo text"
  (cond ((null (and group-id todo-id text))
	 (json-error "Empty params"))
	(t
	 (let ((todo (mito:find-dao 'todos :id todo-id :group-id group-id)))
	   (if (not todo)
	       (json-error "Todo not found")
	       (progn
		 (setf (slot-value todo 'text) text)
		 (mito:save-dao todo)
		 (jonathan:to-json (todo-to-list todo))))))))

(defun api-todos-change-status (group-id todo-id status-id)
  "Change todo status"
  (cond ((null (and group-id todo-id status-id))
	 (json-error "Empty params"))
	((null (mito:retrieve-dao 'statuses :id status-id))
	 (json-error "Bad todo status"))
	(t
	 (let ((todo (mito:find-dao 'todos :id todo-id :group-id group-id)))
	   (if (not todo)
	       (json-error "Todo not found")
	       (progn
		 (setf (slot-value todo 'status-id) status-id)
		 (mito:save-dao todo)
		 (jonathan:to-json (todo-to-list todo))))))))

(defun api-todos-delete (group-id todo-id)
  "Delete todo by id and group"
  (mito:delete-by-values 'todos :id todo-id :group-id group-id)
  (jonathan:to-json (list :id todo-id :group-id group-id)))

(defun api-todos-add (group-name text)
  "Add new todo"
  (if (not (and group-name text))
      (json-error "Empty params")
      (progn
	(when (not (mito:find-dao 'groups :name group-name))
	  (mito:create-dao 'groups :name group-name))
	(let* ((group (mito:find-dao 'groups :name group-name))
	       (todo (mito:create-dao 'todos :group-id (mito:object-id group) :text text :status-id 1)))
	  (if todo
	      (jonathan:to-json (todo-to-list todo))
	      (json-error "Error create todo"))))))

(defun api-todos-get-stats ()
  "Get statistics by todos"
  (let ((statuses (mapcar #'(lambda (status)
			      (list :id (mito:object-id status) :name (statuses-name status)))
			  (mito:select-dao 'statuses)))
	(stats (mito:retrieve-by-sql
		(sxql:select (:group_id
			      (:as :groups.name :group_name)
			      :status_id
			      (:as :statuses.name :status_name)
			      (:as (:count :*) :count))
		  (sxql:left-join :statuses :on (:= :statuses.id :todos.status_id))
		  (sxql:left-join :groups :on (:= :groups.id :todos.group_id))
		  (sxql:group-by :group_id :status_id)
		  (sxql:from :todos)))))
    (jonathan:to-json (list :statuses statuses :stats stats))))

(defun api-todos-export-csv ()
  "Export todos to csv"
  (format nil "~{~{~A~^;~}~^~%~}"
	  `(("id" "group" "status" "text" "date")
	    ,@(mapcar #'(lambda (todo)
			  (list (mito:object-id todo)
				(groups-name (mito:find-dao 'groups :id (todos-group-id todo)))
				(statuses-name (mito:find-dao 'statuses :id (todos-status-id todo)))
				(todos-text todo)
				(local-time:format-timestring nil (mito:object-updated-at todo)
							      :format local-time:+asctime-format+)))
		      (mito:select-dao 'todos (sxql:order-by :group))))))
