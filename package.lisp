;;;; Definition of POLYMORPH.STL package

;;; The package POLYMORPH.STL exports all symbols in the COMMON-LISP
;;; package as well as the symbols in all the POLYMORPH.*
;;; packages. Thus this package can be used in place of the
;;; COMMON-LISP package.

(polymorph.stl.util:define-merged-package #:polymorph.stl
    #:common-lisp
  #:polymorph.copy-cast
  #:polymorph.utility
  #:polymorph.access
  #:polymorph.maths)
