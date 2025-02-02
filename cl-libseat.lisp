
;;  ██████╗██╗      ██╗     ██╗██████╗ ███████╗███████╗ █████╗ ████████╗
;; ██╔════╝██║      ██║     ██║██╔══██╗██╔════╝██╔════╝██╔══██╗╚══██╔══╝
;; ██║     ██║█████╗██║     ██║██████╔╝███████╗█████╗  ███████║   ██║
;; ██║     ██║╚════╝██║     ██║██╔══██╗╚════██║██╔══╝  ██╔══██║   ██║
;; ╚██████╗███████╗ ███████╗██║██████╔╝███████║███████╗██║  ██║   ██║
;;  ╚═════╝╚══════╝ ╚══════╝╚═╝╚═════╝ ╚══════╝╚══════╝╚═╝  ╚═╝   ╚═╝
;; For now - not it's own package
(in-package #:libseat)
(define-foreign-library libseat
  (:unix (:or "libseat.so"))
  (t (:default "libseat")))

(use-foreign-library libseat)
(defvar *help* (load-foreign-library (asdf:output-file 'asdf:compile-op (asdf:find-component :cl-libseat "help"))))

;; ┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┬ ┬┬─┐┌─┐
;; └─┐ │ ├┬┘│ ││   │ │ │├┬┘├┤
;; └─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘┴└─└─┘
(defcenum log-level
  (:silent 0)
  (:error 1)
  (:info 2)
  (:debug 3)
  (:last 4))

(defcstruct libseat-seat-listener
  (enable-seat :pointer)
  (disable-seat :pointer))


;; ┌─┐┬ ┬┌┐┌┌─┐┌┬┐┬┌─┐┌┐┌┌─┐
;; ├┤ │ │││││   │ ││ ││││└─┐
;; └  └─┘┘└┘└─┘ ┴ ┴└─┘┘└┘└─┘
(defcfun ("libseat_open_seat" %open-seat) :pointer
  (listener (:pointer (:struct libseat-seat-listener)))
  (data :pointer))

(defcfun ("libseat_disable_seat" disable-seat) :int
  (seat :pointer))

(defcfun ("libseat_close_seat" %close-seat) :int
  (seat :pointer))

;; Int returned is the device id - this can be used for close-device
(defcfun ("libseat_open_device" %open-device) :int
  (seat :pointer)
  (path :string)
  (fd (:pointer :int)))

(defcfun ("libseat_close_device" close-device) :int
  (seat :pointer)
  (device-id :int))

(defcfun ("libseat_seat_name" seat-name) :string
  (seat :pointer))

(defcfun ("libseat_switch_session" switch-session) :int
  (seat :pointer)
  (session :int))

(defcfun ("libseat_get_fd" get-fd) :int
  (seat :pointer))

(defcfun ("libseat_dispatch" dispatch) :int
  (seat :pointer)
  (timeout :int))

;; Handler
(defcfun ("libseat_set_log_handler" %set-log-handler) :void
  (handler :pointer))

(defcfun ("libseat_set_log_level" set-log-level) :void
  (level log-level))


;; ┌─┐┌─┐┬  ┬  ┌┐ ┌─┐┌─┐┬┌─┌─┐
;; │  ├─┤│  │  ├┴┐├─┤│  ├┴┐└─┐
;; └─┘┴ ┴┴─┘┴─┘└─┘┴ ┴└─┘┴ ┴└─┘
(defcenum pa-flag-mask
  :int
  :char
  :string
  :pointer
  :float
  :double
  :last
  :flag-ptr
  :flag-short
  :flag-long
  :flag-long-long
  :flag-long-double)

;; TODO: Could also translate the c printf format to lisp format i guess but leaving up to implementer
(defvar *log-handler-lisp* nil)
(defcallback log-handler-c :void
  ((level log-level) (format :string)
   ;; va_list in their header. no clue. can't be bothered.
   (data :pointer))
  (funcall *log-handler-lisp* level format data))

(defcallback parsed-callback :void ((format :string) (num-args :int) (arg-types (:pointer pa-flag-mask)) (args :pointer))
  (let ((args
	  (loop for i from 0 below num-args
		for arg-type = (mem-aref arg-types i)
		for arg = (mem-aref args i (case arg-type
					     (:int :int)
					     (:char :char)
					     (:string :string)
					     (:pointer :pointer)
					     (:float :float)
					     (:double :double)
					     (t (error "Unhandled arg type. %s:::I didn't cover bit flags and :last" arg-type))))
		collect arg))))

  (funcall *log-handler-lisp* format args))

(defvar *enable-seat* nil)
(defcallback enable-seat-cb :void ((seat :pointer) (data :pointer))
  (funcall *enable-seat* seat data))

(defvar *disable-seat* nil)
(defcallback disable-seat-cb :void ((seat :pointer) (data :pointer))
  (funcall *disable-seat* seat data))

;; ┬  ┬┌─┐┌─┐
;; │  │└─┐├─┘
;; ┴─┘┴└─┘┴
;; This would try to interface with
(defun default-log-handler (level format data)
  (declare (ignore data))
  (let ((types nil))
    (cl-ppcre:do-scans (match-start match-end reg-starts reg-ends "(%s|%d)" format)
      (case (char format (+ 1 match-start))
	(#\s (push :string types))
	(#\d (push :int types))))

    (format t "~a: ~a~%" level
	    (apply 'format nil
		   (cl-ppcre:regex-replace-all "(%s|%d)" format "~a")
		   (loop for i below (length types)
			 for type in types
			 collect
			 ;; TODO: Accessing va-list arguments directly is not supported in CFFI
			 ;; Would require some extra hackery
			 ;; (case type
			   ;; (:string (mem-aref data :string i))
			   ;; (:int (mem-aref data :int i)))
			 "TODO"
			 )))))


(defun make-libseat-seat-listener (enable-seat-lisp disable-seat-lisp)
  (setf *enable-seat* enable-seat-lisp *disable-seat* disable-seat-lisp)
  (let ((listener (foreign-alloc '(:struct libseat-seat-listener))))
    (with-foreign-slots ((enable-seat disable-seat) listener (:struct libseat-seat-listener))
      (setf enable-seat (callback enable-seat-cb)
	    disable-seat (callback disable-seat-cb)))
    listener))

(defun open-seat (&key enable-seat disable-seat user-data log-handler)
  "Create a new libseat seat context
Two callbacks are required, via the keys
:enable-seat and :disable-seat

:enable-seat is called with the seat context and user-data when the seat is enabled
:disable-seat is called with the seat context and user-data when the seat is disabled

:user-data is a pointer to user data which will be passed to the callbacks,
the creation of the pointer is left up to the user.
If :user-data is not provided a null-pointer is used.

Additionally a log-handler can be provided, if T the default log handler (printf) is used
Use at your own risk - the function receives variadic arguments which aren't supported
by cffi (i should probably disable that feature)
See default-log-handler for a partial implementation lacking variadic parsing"

  (unless enable-seat (error "enable-seat callback required"))
  (unless disable-seat (error "disable-seat callback required"))
  (etypecase log-handler
    ((eql T) (set-log-handler nil :callback (foreign-symbol-pointer "log_printf" :library *help*)))
    (function (set-log-handler log-handler :callback (foreign-symbol-pointer "parse_and_callback" :library *help*))))

  (let ((seat (%open-seat (make-libseat-seat-listener enable-seat disable-seat) (or user-data (null-pointer)))))
    (if (null-pointer-p seat) nil seat)))

(defun close-seat (seat) (%close-seat seat))

(defun open-device (seat path)
  (with-foreign-object (fd :int)
    (let ((device-id (%open-device seat path fd)))
      (if (>= device-id 0)
	  (values device-id (mem-ref fd :int))
	  (error (format nil "Failed to open device. Error: ~a" device-id))))))

(defun set-log-handler (handler &key callback)
  (when handler (setf *log-handler-lisp* handler))
  (%set-log-handler (or callback (callback log-handler-c))))
