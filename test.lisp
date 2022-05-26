;;; Run unit tests for all lisp-polymorph systems

(defpackage #:polymorph.stl/test
  (:use #:cl
        #:fiveam
        #:polymorph.maths/test
        #:polymorph.access/test
        #:polymorph.copy-cast/test)
        ;#:polymorph.callable/test)

  (:export #:test-polymorph.stl))

(in-package #:polymorph.stl/test)

(defun test-polymorph.stl ()
  "Run tests for all lisp-polymorph systems"

  (explain!
   (append
    (run 'polymorph.maths)
    (run 'polymorph.access)
    (run 'polymorph.copy-cast))))
    ;(run 'polymorph.callable))))
