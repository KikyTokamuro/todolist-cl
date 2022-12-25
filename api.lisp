;;;; api.lisp

(in-package #:todolist)

(defun string-to-keyword (str)
  "String -> :KEYWORD"
  (values (intern (string-upcase str) "KEYWORD")))

(defun json-error (message)
  "Generate JSON error"
  (jonathan:to-json (list :error message)))

(defun api-group-list ()
  "Return list of groups"
  (jonathan:to-json (mapcar #'(lambda (group)
				(list :id (mito:object-id group) :name (groups-name group)))
			    (mito:select-dao 'groups))))

(defun api-group-delete (group-name)
  "Delete group"
  (mito:delete-by-values 'groups :name group-name)
  (mito:delete-by-values 'todos :group group-name)
  (jonathan:to-json '(:group group-name)))

(defun api-todos-all ()
  "Return all todos"
  (let ((response '()))
    (loop for todo in (mito:select-dao 'todos)
	  do (let* ((group-name (todos-group todo))
		    (group-name-sym (string-to-keyword group-name))
		    (response-element (getf response group-name-sym))
		    (todo-info (list :id (mito:object-id todo)
				     :status (todos-status todo)
				     :text (todos-text todo)
				     :date (local-time:format-timestring nil (mito:object-created-at todo)
									 :format local-time:+asctime-format+))))
	       (if response-element
		   (setf (getf response group-name-sym) (append response-element (list todo-info)))
		   (setf (getf response group-name-sym) (list todo-info)))))
    (jonathan:to-json response)))

(defun api-todos-by-group (group-name)
  "Return todos from group"
  (jonathan:to-json
   (if group-name
       (mapcar #'(lambda (todo)
		   (list :id (mito:object-id todo)
			 :status (todos-status todo)
			 :text (todos-text todo)
			 :date (local-time:format-timestring nil (mito:object-created-at todo)
							     :format local-time:+asctime-format+)))
	       (mito:retrieve-dao 'todos :group group-name))
       '())))

(defun api-todos-by-group-and-id (group-name todo-id)
  "Return todo by group and id"
  (cond ((null (and group-name todo-id))
	 (json-error "Empty params"))
	(t
	 (let ((todo (mito:find-dao 'todos :id todo-id :group group-name)))
	   (if (not todo)
	       (json-error "Todo not found")
	       (progn
		 (jonathan:to-json (list :id (mito:object-id todo)
					 :status (todos-status todo)
					 :text (todos-text todo)
					 :date (local-time:format-timestring nil (mito:object-created-at todo)
									     :format local-time:+asctime-format+)))))))))

(defun api-todos-change-status (group-name todo-id status)
  "Change todo status"
  (cond ((null (and group-name todo-id status))
	 (json-error "Empty params"))
	((null (find status '("todo" "doing" "done") :test #'string=))
	 (json-error "Bad todo status"))
	(t
	 (let ((todo (mito:find-dao 'todos :id todo-id :group group-name)))
	   (if (not todo)
	       (json-error "Todo not found")
	       (progn
		 (setf (slot-value todo 'status) status)
		 (mito:save-dao todo)
		 (jonathan:to-json (list :id (mito:object-id todo)
					 :status (todos-status todo)
					 :text (todos-text todo)
					 :date (local-time:format-timestring nil (mito:object-created-at todo)
									     :format local-time:+asctime-format+)))))))))

(defun api-todos-delete (group-name todo-id)
  "Delete todo by id and group"
  (mito:delete-by-values 'todos :id todo-id :group group-name)
  (jonathan:to-json (list :id todo-id :group group-name)))

(defun api-todos-add (group-name text)
  "Add new todo"
  (if (not (and group-name text))
      (json-error "Empty params")
      (progn
	(when (not (mito:find-dao 'groups 'name group-name))
	  (mito:create-dao 'groups :name group-name))
	(let ((todo (mito:create-dao 'todos :group group-name :text text :status "todo")))
	  (if todo
	      (jonathan:to-json (list :id (mito:object-id todo)
				      :group (todos-group todo)
				      :status (todos-status todo)
				      :text (todos-text todo)
				      :date (local-time:format-timestring nil (mito:object-created-at todo)
									  :format local-time:+asctime-format+)))
	      (json-error "Error create todo"))))))

(defun api-todos-get-stats ()
  "Get statistics by todos"
  (let ((response '())
	(todos-stats (mito:retrieve-by-sql
		      (sxql:select (:group :status (:as (:count :*) :count))
			(sxql:group-by :group :status)
			(sxql:from (sxql:make-sql-symbol (mito.class.table:table-name (find-class 'todos))))))))
    (loop for stats in todos-stats
	  do (let* ((group-name-sym (string-to-keyword (getf stats ':group)))
		    (status-sym (string-to-keyword (getf stats ':status)))
		    (count (getf stats ':count))
		    (group-origname (substitute #\space #\_ (getf stats ':group))))
	       (if (not (getf response group-name-sym))
		   (setf (getf response group-name-sym) (list :todo 0 :doing 0 :done 0 :origname group-origname)))
	       (setf (getf (getf response group-name-sym) status-sym) count)))
    (jonathan:to-json response)))

(defun api-todos-export-csv ()
  "Export todos to csv"
  (format nil "~{~{~A~^;~}~^~%~}"
	  `(("id" "group" "status" "text" "date")
	    ,@(mapcar #'(lambda (todo)
			  (list (mito:object-id todo)
				(todos-group todo)
				(todos-status todo)
				(todos-text todo)
				(local-time:format-timestring nil (mito:object-created-at todo)
							      :format local-time:+asctime-format+)))
		      (mito:select-dao 'todos (sxql:order-by :group))))))
