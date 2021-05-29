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

; transation controls
(defparameter translate-left-button nil)
(defparameter translate-right-button nil)
(defparameter translate-up-button nil)
(defparameter translate-down-button nil)
(defparameter translate-forward-button nil)
(defparameter translate-backward-button nil)

; readings
(defparameter reading-roll nil)
(defparameter reading-roll-rate nil)
(defparameter reading-pitch nil)
(defparameter reading-pitch-rate nil)
(defparameter reading-yaw nil)
(defparameter reading-yaw-rate nil)

(defparameter reading-translate-x nil)
(defparameter reading-translate-y nil)
(defparameter reading-translate-z nil)
(defparameter reading-translate-rate nil)
(defparameter reading-translate-range nil)



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
(defvar payload-find-elem-by-selector "{\"using\":\"css selector\",\"value\":\"SELECTOR\"}")
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

(defmethod find-elem-by-selector ((obj Driver) &key selector)
  "Select and Create an elem by css selector"
  (let ((resp (dex:post
              (format-url (url obj) "session" (session-id obj) "element")
              :content (string-replace payload-find-elem-by-selector "SELECTOR" selector))))
    (make-instance 'Elem 
                   :selector selector
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
  (selector
     :initarg :selector
     :reader selector)
   (dom-id
     :initarg :dom-id
     :reader dom-id))
  (:documentation "ELement in DOM"))

(defmethod click ((obj Elem))
  (dex:post
    (format-url (session-url obj) "element" (dom-id obj) "click")
    :content (string-replace payload-click "ID" (dom-id obj))))
;
(defmethod text ((obj Elem))
  (let ((resp 
          (dex:get
            (format-url (session-url obj) "element" (dom-id obj) "text"))))
    (cdr (car (string-to-cl resp)))))


(defmethod print-object ((obj Elem) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a Id:~a" (selector obj) (dom-id obj))))

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
        (click (find-elem-by-selector driver :selector "#begin-button")))
      d)))


(defun begin (driver)
  (princ (session-id driver)))

(defun init-controllers (driver) 
  (let (
        ; rotation controls
        (yaw-left-btn (find-elem-by-selector driver :selector "#yaw-left-button"))
        (yaw-right-btn (find-elem-by-selector driver :selector "#yaw-right-button"))
        (pitch-up-btn (find-elem-by-selector driver :selector "#pitch-up-button"))
        (pitch-down-btn (find-elem-by-selector driver :selector "#pitch-down-button"))
        (roll-left-btn (find-elem-by-selector driver :selector "#roll-left-button"))
        (roll-right-btn (find-elem-by-selector driver :selector "#roll-right-button"))

        ; transation controls
        (translate-left-btn (find-elem-by-selector driver :selector "#translate-left-button"))
        (translate-right-btn (find-elem-by-selector driver :selector "#translate-right-button"))
        (translate-up-btn (find-elem-by-selector driver :selector "#translate-up-button"))
        (translate-down-btn (find-elem-by-selector driver :selector "#translate-down-button"))
        (translate-forward-btn (find-elem-by-selector driver :selector "#translate-forward-button"))
        (translate-backward-btn (find-elem-by-selector driver :selector "#translate-backward-button"))

        ; readings
        (reading-roll-div (find-elem-by-selector driver :selector "#roll > div.error"))
        (reading-roll-rate-div (find-elem-by-selector driver :selector "#roll > div.rate"))
        (reading-pitch-div (find-elem-by-selector driver :selector "#pitch > div.error"))
        (reading-pitch-rate-div (find-elem-by-selector driver :selector "#pitch> div.rate"))
        (reading-yaw-div (find-elem-by-selector driver :selector "#yaw > div.error"))
        (reading-yaw-rate-div (find-elem-by-selector driver :selector "#yaw > div.rate"))

        (reading-translate-x-div (find-elem-by-selector driver :selector "#x-range > div.distance"))
        (reading-translate-y-div (find-elem-by-selector driver :selector "#y-range > div.distance"))
        (reading-translate-z-div (find-elem-by-selector driver :selector "#z-range > div.distance"))
        (reading-translate-rate-div (find-elem-by-selector driver :selector "#rate > div.rate"))
        (reading-translate-range-div (find-elem-by-selector driver :selector "#range > div.rate"))
        )
    (progn
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

      (setq reading-roll reading-roll-div)
      (setq reading-roll-rate reading-roll-rate-div)
      (setq reading-pitch reading-pitch-div)
      (setq reading-pitch-rate reading-pitch-rate-div)
      (setq reading-yaw reading-yaw-div)
      (setq reading-yaw-rate reading-yaw-rate-div)

      (setq reading-translate-x reading-translate-x-div)
      (setq reading-translate-y reading-translate-y-div)
      (setq reading-translate-z reading-translate-z-div)
      (setq reading-translate-rate reading-translate-rate-div)
      (setq reading-translate-range reading-translate-range-div)

      )))

(defun main ()
  (let ((sim (init-sim)))
    (sleep 5) ; Wait for begin annimation
    (init-controllers sim)
    ))
;(main)



























