;;;; polymorph.stl.asd

(asdf:defsystem #:polymorph.stl
  :description "All polymorph systems united"
  :license  "MIT"
  :version "0.2"
  :serial t
  :components ((:file "package"))

  :depends-on (#:polymorph.copy-cast
               #:polymorph.access
               #:polymorph.maths
               #:polymorph.macros
               #:polymorph.traversable
               #:polymorph.callable)
               ;#:polymorph.data-structures)

  :in-order-to ((asdf:test-op (asdf:test-op :polymorph.stl/test))))

(asdf:defsystem #:polymorph.stl/test
  :description "Unit tests for polymorph.stl"
  :serial t
  :depends-on (#:polymorph.stl
               #:polymorph.maths/test
               #:polymorph.access/test
               #:polymorph.copy-cast/test
               #:polymorph.callable/test
               #:fiveam)

  :components ((:file "test"))

  :perform (test-op (o s)
                    (uiop:symbol-call '#:polymorph.stl/test '#:test-polymorph.stl)))
