
;;  ██████╗██╗      ██╗     ██╗██████╗ ███████╗███████╗ █████╗ ████████╗
;; ██╔════╝██║      ██║     ██║██╔══██╗██╔════╝██╔════╝██╔══██╗╚══██╔══╝
;; ██║     ██║█████╗██║     ██║██████╔╝███████╗█████╗  ███████║   ██║
;; ██║     ██║╚════╝██║     ██║██╔══██╗╚════██║██╔══╝  ██╔══██║   ██║
;; ╚██████╗███████╗ ███████╗██║██████╔╝███████║███████╗██║  ██║   ██║
;;  ╚═════╝╚══════╝ ╚══════╝╚═╝╚═════╝ ╚══════╝╚══════╝╚═╝  ╚═╝   ╚═╝
(asdf:defsystem #:cl-libseat
  :description "Common Lisp wrapper for libseat"
  :author ("bmiww <bmiww@bky.one>")
  :license "BSD 3-Clause"
  :depends-on (#:cffi #:cl-ppcre)
  :serial t
  :components ((:file "package")
	       (:file "cl-libseat")))
