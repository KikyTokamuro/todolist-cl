;;;; index.lisp

(in-package #:todolist)

(defun version ()
  "Get version of this project"
  (slot-value (asdf:find-system 'todolist) 'asdf:version))

(defun index-page ()
  "Generate index page html"
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "todolist")
      (:link :rel "stylesheet" :href "./static/style.css")
      (:link :rel "stylesheet" :href "./static/libs/jquery-ui.css")
      (:link :rel "stylesheet" :href "./static/libs/liner-bar.css")
      (:link :rel "stylesheet" :href "./static/libs/quill.snow.min.css")
      (:link :rel "icon" :type "image/x-icon" :href "./static/images/favicon.ico")
      (:script :src "./static/libs/jquery.min.js")
      (:script :src "./static/libs/jquery-ui.min.js")
      (:script :src "./static/libs/quill.min.js")
      (:script :src "./static/libs/liner-bar.js"))
     (:body
      (:div :class "todolist-wrapper"
	    (:div :class "todolist-tools"
		  (:a :id "generate-csv" :href "/api/generate/csv"
		      (:img :src "./static/images/csv.svg"))
		  (:div :id "statistics"
		      (:img :src "./static/images/stats.svg"))
		  (:a :href "https://github.com/KikyTokamuro/todolist-cl" :target "_blank"
		      (version)))
	    (:div :class "todolist-groups-wrapper"
		  (:div :class "todolist-group-button" :id "group-all"
			(:span "all")))
	    (:div :class "todolist-search-wrapper"
		  (:input :type "text" :id "search-input" :placeholder "Insert search filter"))
	    (:div :class "todolist-body"
		  (:div :class "todolist-todo-column"
			(:div :class "todolist-todo-column-title" "TODO")
			(:div :class "column todolist-todo-column-body"))
		  (:div :class "todolist-doing-column"
			(:div :class "todolist-doing-column-title" "DOING")
			(:div :class "column todolist-doing-column-body"))
		  (:div :class "todolist-done-column"
			(:div :class "todolist-done-column-title" "DONE")
			(:div :class "column todolist-done-column-body")))
	    (:div :class "todolist-create-button"
		  (:img :src "./static/images/pencil.svg")))
	    ;; (:div :class "todolist-input-wrapper"
	    ;; 	  (:textarea :id "task-input" :rows "1" :placeholder "Insert task")
	    ;; 	  (:div :class "separator")
	    ;; 	  (:input :type "text" :id "task-group" :placeholder "Insert task group")
	    ;; 	  (:div :class "separator")
	    ;; 	  (:div :class "send-task-button"
	    ;; 		(:img :src "./static/images/send.svg"))))
      (:div :class "todolist-create-task-modal" :style "display:none" :title "New task")
      (:div :class "todolist-edit-task-modal" :style "display:none" :title "Edit task")
      (:div :class "todolist-statistics-modal" :style "display:none" :title "Statistics")
      (:script :type "module" :src "./static/app.js")))))
