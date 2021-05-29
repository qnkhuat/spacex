(ql:quickload "dexador")
(ql:quickload "cl-json")

; *** Global variables ***
; Easier for testing
(defparameter driver nil)

; Rotation controls
(defparameter yaw-left-button nil)
(defparameter yaw-right-button nil)
(defparameter pitch-up-button nil)
(defparameter pitch-down-button nil)
(defparameter roll-left-button nil)
(defparameter roll-right-button nil)

;transation controls
(defparameter translate-left-button nil)
(defparameter translate-right-button nil)
(defparameter translate-up-button nil)
(defparameter translate-down-button nil)
(defparameter translate-forward-button nil)
(defparameter translate-backward-button nil)


; *** Utils ***
(defvar f "spacex.lisp")
(defvar spacex-sim-url "https://iss-sim.spacex.com/")
(defun l (name) (load name))
(defun string-replace (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
  is replaced with replacement."
  (let ((copy-string string))
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part copy-string
                              :start2 old-pos
                              :test test)
            do (write-string copy-string out
                             :start old-pos
                             :end (or pos (length copy-string)))
            when pos do (write-string replacement out)
            while pos))))

(defun format-url (&rest list) (format nil "~{~A~^/~}" list))

(defun string-to-cl (s)
  "Convert a JSON string to Lisp object"
  (with-input-from-string (sc s)
    (json:decode-json sc)))

; *** Payload Template ***
(defvar payload-init-session "{\"capabilities\":{\"firstMatch\":[{}],\"alwaysMatch\":{\"browserName\":\"chrome\",\"platformName\":\"any\",\"goog:chromeOptions\":{\"extensions\":[],\"args\":[]}}}}")
(defvar payload-goto "{\"url\":\"URL\"}")
(defvar payload-find-elem-by-id "{\"using\":\"css selector\",\"value\":\"[id=\\\"ID\\\"]\"}")
(defvar payload-click "{\"id\":\"ID\"}")

; *** Classes *** 
; Driver to control Chrome WebDriver
(defclass Driver ()
  ((url
     :initarg :url
     :reader url
     :documentation "WebDriver url"
     )
   (session-id 
     :initform nil
     :accessor session-id))
  (:documentation "Driver to control WebDriver"))

(defmethod init-session ((obj Driver))
  (let ((resp (dex:post
                 (format-url (url obj) "session")
                 :content payload-init-session)))
    (setf (session-id obj) (cdr (car (cdr (cdr (car (string-to-cl resp)))))))))

(defmethod goto ((obj Driver) &key dest)
  "Open an URL on webdriver"
  (dex:post
    (format-url (url obj) "session" (session-id obj) "url")
    :content (string-replace payload-goto "URL" dest)))

(defmethod find-elem-by-id ((obj Driver) &key id)
  "Select and Create an elem by id"
  (let ((resp (dex:post
              (format-url (url obj) "session" (session-id obj) "element")
              :content (string-replace payload-find-elem-by-id "ID" id))))
    (make-instance 'Elem 
                   :id id
                   :session-url (format-url (url driver) "session" (session-id driver))
                   :dom-id (cdr (car (cdr (car (string-to-cl resp)))))
                   )))

(defmethod print-object ((obj Driver) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((driver-url url)
                     (driver-session-id session-id))
      obj
      (format stream "~a, SessionId: ~a" driver-url driver-session-id))))

(defun make-driver (&key (host "http://127.0.0.1") (port "9515"))
  (let ((url (concatenate 'string host ":" port)))
    (make-instance 'Driver :url url)))

; Element in DOM
(defclass Elem ()
  ((session-url
     :initarg :session-url
     :reader session-url)
  (id
     :initarg :id
     :reader id)
   (dom-id
     :initarg :dom-id
     :reader dom-id))
  (:documentation "ELement in DOM"))

(defmethod click ((obj Elem))
  (dex:post
    (format-url (session-url obj) "element" (dom-id obj) "click")
    :content (string-replace payload-click "ID" (dom-id obj))))
;
(defmethod print-object ((obj Elem) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a Id:~a" (id obj) (dom-id obj))))

(defun make-driver (&key (host "http://127.0.0.1") (port "9515"))
  (let ((url (concatenate 'string host ":" port)))
    (make-instance 'Driver :url url)))


; *** Main ***
(defun init-sim() 
  (let ((d (make-driver)))
    (progn
      (setq driver d)
      (init-session d)
      (goto d :dest spacex-sim-url)
      (handler-bind ((dex:http-request-failed #'dex:retry-request))
        (click (find-elem-by-id driver :id "begin-button")))
      d)))


(defun begin (driver)
  (princ (session-id driver)))

(defun init-controllers (driver) 
  (let (
        ; rotation controls
        (yaw-left-btn (find-elem-by-id driver :id "yaw-left-button"))
        (yaw-right-btn (find-elem-by-id driver :id "yaw-right-button"))
        (pitch-up-btn (find-elem-by-id driver :id "pitch-up-button"))
        (pitch-down-btn (find-elem-by-id driver :id "pitch-down-button"))
        (roll-left-btn (find-elem-by-id driver :id "roll-left-button"))
        (roll-right-btn (find-elem-by-id driver :id "roll-right-button"))

        ; transation controls
        (translate-left-btn (find-elem-by-id driver :id "translate-left-button"))
        (translate-right-btn (find-elem-by-id driver :id "translate-right-button"))
        (translate-up-btn (find-elem-by-id driver :id "translate-up-button"))
        (translate-down-btn (find-elem-by-id driver :id "translate-down-button"))
        (translate-forward-btn (find-elem-by-id driver :id "translate-forward-button"))
        (translate-backward-btn (find-elem-by-id driver :id "translate-backward-button"))
        )
    (progn
      (princ "Yawn left")
      (princ driver)
      (princ yaw-left-button)
      (setq yaw-left-button yaw-left-btn)
      (setq yaw-right-button yaw-right-btn)
      (setq pitch-up-button pitch-up-btn)
      (setq pitch-down-button pitch-down-btn)
      (setq roll-left-button roll-left-btn)
      (setq roll-right-button roll-right-btn)

      (setq translate-left-button translate-left-btn)
      (setq translate-right-button translate-right-btn)
      (setq translate-up-button translate-up-btn)
      (setq translate-down-button translate-down-btn)
      (setq translate-forward-button translate-forward-btn)
      (setq translate-backward-button translate-backward-btn)
      )))

(defun main ()
  (let ((sim (init-sim)))
    (sleep 5) ; Wait for begin annimation
    (init-controllers sim)
    ))
;(main)



























