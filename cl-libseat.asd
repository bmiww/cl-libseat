
;;  ██████╗██╗      ██╗     ██╗██████╗ ███████╗███████╗ █████╗ ████████╗
;; ██╔════╝██║      ██║     ██║██╔══██╗██╔════╝██╔════╝██╔══██╗╚══██╔══╝
;; ██║     ██║█████╗██║     ██║██████╔╝███████╗█████╗  ███████║   ██║
;; ██║     ██║╚════╝██║     ██║██╔══██╗╚════██║██╔══╝  ██╔══██║   ██║
;; ╚██████╗███████╗ ███████╗██║██████╔╝███████║███████╗██║  ██║   ██║
;;  ╚═════╝╚══════╝ ╚══════╝╚═╝╚═════╝ ╚══════╝╚══════╝╚═╝  ╚═╝   ╚═╝
;; NOTE: The code for the c helper blindly stolen from the cl-unix-sockets library
(defpackage :libseat-asdf
  (:use :cl :asdf))
(in-package :libseat-asdf)

(defclass lib-source-file (c-source-file)
  ())

(defparameter *library-file-dir*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defmethod output-files ((o compile-op) (c lib-source-file))
  (let ((library-file-type
          (funcall (intern (symbol-name'#:default-foreign-library-type)
                           (symbol-name '#:uffi)))))
    (list (make-pathname :name (component-name c)
                         :type library-file-type
                         :defaults *library-file-dir*))))

(defmethod perform ((o load-op) (c lib-source-file)) t)

(defmethod perform ((o compile-op) (c lib-source-file))
  (uiop:run-program (list "/usr/bin/env" "gcc" "-shared" "-o" (namestring (car (output-files o c)))
                          "-Werror"
                          "-fPIC"
                          (namestring
                           (merge-pathnames (format nil "~a.c" (component-name c))
                                            *library-file-dir*)))
                    :output :interactive
                    :error-output :interactive))


(asdf:defsystem #:cl-libseat
  :description "Common Lisp wrapper for libseat"
  :author ("bmiww <bmiww@bky.one>")
  :license "BSD 3-Clause"
  :defsystem-depends-on (:uffi #| todo: remove |#)
  :depends-on (#:cffi #:cl-ppcre)
  :serial t
  :components ((:file "package")
	       (lib-source-file "help")
	       (:file "cl-libseat")))
