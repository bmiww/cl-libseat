
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
(defcfun ("libseat_open_seat" open-seat) :pointer
  (listener (:pointer (:struct libseat-seat-listener)))
  (data :pointer))

(defcfun ("libseat_disable_seat" disable-seat) :int
  (seat :pointer))

(defcfun ("libseat_close_seat" close-seat) :int
  (seat :pointer))

;; Int returned is the device id - this can be used for close-device
(defcfun ("libseat_open_device" *open-device) :int
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
;; TODO: Could also translate the c printf format to lisp format i guess but leaving up to implementer
(defvar *log-handler-lisp* nil)
(defcallback log-handler-c :void
  ((level log-level) (format :string)
   ;; va_list in their header. no clue. can't be bothered.
   (data :pointer))
  (funcall *log-handler-lisp* level format data))


;; ┬  ┬┌─┐┌─┐
;; │  │└─┐├─┘
;; ┴─┘┴└─┘┴
(defun open-device (seat path)
  (with-foreign-object (fd :int)
    (let ((device-id (open-device seat path fd)))
      (if (>= device-id 0)
	  (values device-id fd)
	  (error "Failed to open device")))))

(defun set-log-handler (handler)
  (setf *log-handler-lisp* handler)
  (%set-log-handler (callback handler)))
