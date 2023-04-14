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
	    (:div :class "todolist-header"
		  (:div :class "todolist-logo"
			(:a :href "https://github.com/KikyTokamuro/todolist-cl" :target "_blank"
			    (:span :class "todolist-logo-text" "todolist-cl")
			    (version)))
		  (:div :class "todolist-tools"
			(:a :id "generate-csv" :href "/api/generate/csv"
			    (:img :src "./static/images/csv.svg"))
			(:div :id "statistics"
			      (:img :src "./static/images/stats.svg"))))
	    (:div :class "todolist-groups-wrapper"
		  (:div :class "todolist-group-button" :group "all"
			(:span "all")))
	    (:div :class "todolist-search-wrapper"
		  (:input :type "text" :id "search-input" :placeholder "Insert search filter"))
	    (:div :class "todolist-body"
		  (dolist (status (mito:retrieve-dao 'statuses))
		    (:div :class "todolist-column"
			  (:div :class "todolist-column-title" (statuses-name status))
			  (:div :class "column todolist-column-body" :status (mito:object-id status)))))
	    (:div :class "todolist-create-button"
		  (:img :src "./static/images/pencil.svg")))
      (:div :class "todolist-create-task-modal" :style "display:none" :title "New task"
	    (:div :class "todolist-task-editor")
	    (:div :class "todolist-input-wrapper"
		  (:input :type "text" :id "task-group" :placeholder "Insert task group")
		  (:div :class "send-task-button"
			(:img :src "./static/images/send.svg")
			"Create")))
      (:div :class "todolist-edit-task-modal" :style "display:none" :title "Edit task"
	    (:div :class "todolist-task-editor")
	    (:div :class "edit-task-button"
		  (:img :src "./static/images/save.svg")
		  "Save changes"))
      (:div :class "todolist-statistics-modal" :style "display:none" :title "Statistics")
      (:div :class "todolist-error-modal" :style "display:none" :title "Error"
	    (:div :class "todolist-error-text"))
      (:script :type "module" :src "./static/app.js")))))
