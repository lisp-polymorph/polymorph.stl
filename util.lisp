;;;; Package Definition Utility Macros

(defpackage #:polymorph.stl.util
  (:use #:cl)
  (:export #:define-merged-package))

(in-package #:polymorph.stl.util)

(defun merge-packages (new-package-name &rest packages)
  "Creates a new package with name NEW-PACKAGE-NAME, if it does not
   already exist, into which all external symbols in each package in
   PACKAGES are imported, by SHADOWING-IMPORT. If the package is a
   list with :INTERNAL as the first element, the internal symbols of
   the package (with the name as the second element) are imported
   instead.

   The external symbols of each package in PACKAGES are imported in
   the order in which the package appears in the list, thus symbols
   imported from packages towards the end of the PACKAGES list will
   shadow symbols imported from packages at the beginning of the
   list."

  (flet ((merge-package (pkg)
	   (etypecase pkg
	     (list
	      (if (eq (first pkg) :internal)
		  (do-symbols (sym (second pkg))
		    (shadowing-import (list sym)))

		  (error "Expected SYMBOL or (:INTERNAL SYMBOL) for package name in MERGE-PACKAGES.")))

	     (symbol
	      (do-external-symbols (sym pkg)
		(shadowing-import (list sym))
		(export (list sym)))))))

    (let ((*package* (or (find-package new-package-name) (make-package new-package-name :use nil))))
      (mapc #'merge-package packages)
      *package*)))

(defmacro define-merged-package (name &rest packages)
  "Convenience macro which defines a merged package using
   MERGE-PACKAGES. NAME (not evaluated) is the name of the new package
   and PACKAGES (not evaluated) is the list of packages of which the
   external symbols are imported in package NAME."

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (apply #'merge-packages ',name ',packages)))
