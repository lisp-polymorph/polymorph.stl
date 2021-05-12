;;;; polymorph.stl.asd

(asdf:defsystem #:polymorph.stl
  :description "Describe polymorph.stl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package"))

  :depends-on (#:polymorph.copy-cast
	       #:polymorph.utility
	       #:polymorph.access
	       #:polymorph.maths))
