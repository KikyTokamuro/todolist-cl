;;;; todolist.asd

(asdf:defsystem #:todolist
  :description "Todolist with web UI"
  :author "Daniil Archangelsky <kiky.tokamuro@yandex.ru>"
  :license  "MIT"
  :version "2.0.1"
  :serial t
  :depends-on (#:hunchentoot #:spinneret #:jonathan #:mito)
  :components ((:file "package")
	       (:file "routes")
               (:file "todolist")
	       (:file "database")
	       (:file "index")
	       (:file "api")))
