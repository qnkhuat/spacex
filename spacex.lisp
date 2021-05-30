(ql:quickload "dexador")
(ql:quickload "cl-json")

; *** Global variables ***
; Easier for testing
(defvar driver nil)

; Rotation controls
(defvar yaw-left-button nil)
(defvar yaw-right-button nil)
(defvar pitch-up-button nil)
(defvar pitch-down-button nil)
(defvar roll-left-button nil)
(defvar roll-right-button nil)

; transation controls
(defvar translate-left-button nil)
(defvar translate-right-button nil)
(defvar translate-up-button nil)
(defvar translate-down-button nil)
(defvar translate-forward-button nil)
(defvar translate-backward-button nil)

; readings
(defvar reading-roll nil)
(defvar reading-roll-rate nil)
(defvar reading-pitch nil)
(defvar reading-pitch-rate nil)
(defvar reading-yaw nil)
(defvar reading-yaw-rate nil)

(defvar reading-translate-x nil)
(defvar reading-translate-y nil)
(defvar reading-translate-z nil)
(defvar reading-translate-rate nil)
(defvar reading-translate-range nil)
(defvar prev-roll 0)



; *** Utils ***
(defvar f "spacex.lisp")
(defvar spacex-sim-url "https://iss-sim.spacex.com/")
(defun l (name) (load name))

(defun string-sub-any (string list)
  "subsitute any char in list from string"
  (if (= (length list) 0)
    string
    (string-sub-any (remove (char list 0) string) (subseq list 1))))

(defun text-to-value (text)
  (with-input-from-string (s (string-sub-any text "°/sm "))  (read s)))

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

(defun string-to-cl (s)
  "Convert a JSON string to Lisp object"
  (with-input-from-string (sc s)
    (json:decode-json sc)))

(defun format-url (&rest list) (format nil "~{~A~^/~}" list))

(defun current-date-string ()
  "Returns current date as a string."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
    (get-decoded-time)
    (declare (ignore sec min hr dow dst-p tz))(format nil "~d:~d:~d" hr min sec))
  )

(defun call-n-times (func n)
  (if (< n 1)
    'done
    (progn
      (funcall func)
      (call-n-times func (- n 1)))))


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

; *** Controllers ***
(defun bound-target (target max-rate)
  (cond ((and (< target 0) (< target (- max-rate)))
         (- max-rate))
        ((and (> target 0) (> target max-rate))
         max-rate)
        (t target)))

(defun fine-tune (&key value rate max-rate dec-btn inc-btn jump)
  (let ((target (bound-target value max-rate)))
    (if (> rate target)
      (call-n-times (lambda() (click dec-btn)) (round (* (- rate target) (/ 1 jump))))
      (call-n-times (lambda() (click inc-btn)) (round (* (- target rate) (/ 1 jump))))
      )))

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
      (if (null reading-translate-range)
        (init-controllers driver)
        'done
        )
      )))


(defun print-status()
  (let
    (
     (roll (text-to-value (text reading-roll)))
     (roll-rate (text-to-value (text reading-roll-rate)))
     (pitch (text-to-value (text reading-pitch)))
     (pitch-rate (text-to-value (text reading-pitch-rate)))
     (yaw (text-to-value (text reading-yaw)))
     (yaw-rate (text-to-value (text reading-yaw-rate)))
     (x (text-to-value (text reading-translate-x)))
     (y (text-to-value (text reading-translate-y)))
     (z (text-to-value (text reading-translate-z)))
     (rate (text-to-value (text reading-translate-rate)))
     )
    (progn
      (princ "------------------------")
      (terpri)
      (format t "Time: ~s" (current-date-string))
      (terpri)
      (format t "Roll: ~d, rate: ~d" roll roll-rate)
      (terpri)
      (format t "Pitch ~d, rate: ~d" pitch pitch-rate)
      (terpri)
      (format t "Yaw ~d, rate: ~d" yaw yaw-rate)
      (terpri)
      (format t "x: ~d" x)
      (terpri)
      (format t "y: ~d" y)
      (terpri)
      (format t "z: ~d" z)
      (terpri)
      (format t "rate: ~d" rate)
      (terpri)
      )
    ))

(defun autopilot
  (
   yaw-left-button 
   yaw-right-button 
   pitch-up-button 
   pitch-down-button 
   roll-left-button 
   roll-right-button 

   translate-left-button 
   translate-right-button 
   translate-up-button 
   translate-down-button 
   translate-forward-button 
   translate-backward-button 

   reading-roll-div
   reading-roll-rate-div
   reading-pitch-div
   reading-pitch-rate-div
   reading-yaw-div
   reading-yaw-rate-div
   reading-translate-x-div
   reading-translate-y-div
   reading-translate-z-div
   reading-translate-rate-div
   )

  (let (
        (roll (text-to-value (text reading-roll-div)))
        (roll-rate (text-to-value (text reading-roll-rate-div)))
        (pitch (text-to-value (text reading-pitch-div)))
        (pitch-rate (text-to-value (text reading-pitch-rate-div)))
        (yaw (text-to-value (text reading-yaw-div)))
        (yaw-rate (text-to-value (text reading-yaw-rate-div)))
        (x (text-to-value (text reading-translate-x-div)))
        (y (text-to-value (text reading-translate-y-div)))
        (z (text-to-value (text reading-translate-z-div)))
        (rate (text-to-value (text reading-translate-rate-div)))
        )
    (progn
      (print-status)

      (sleep .1)
      (fine-tune :value roll :rate roll-rate :max-rate .4 :dec-btn roll-left-button :inc-btn roll-right-button :jump .1)
      (fine-tune :value pitch :rate pitch-rate :max-rate .4 :dec-btn pitch-up-button :inc-btn pitch-down-button :jump .1)
      (fine-tune :value yaw :rate yaw-rate :max-rate .4 :dec-btn yaw-left-button :inc-btn yaw-right-button :jump .1)
      (autopilot
        yaw-left-button 
        yaw-right-button 
        pitch-up-button 
        pitch-down-button 
        roll-left-button 
        roll-right-button 

        translate-left-button 
        translate-right-button 
        translate-up-button 
        translate-down-button 
        translate-forward-button 
        translate-backward-button 

        reading-roll
        reading-roll-rate
        reading-pitch
        reading-pitch-rate
        reading-yaw
        reading-yaw-rate

        reading-translate-x
        reading-translate-y
        reading-translate-z
        reading-translate-rate
        )
      )))

(defun ap ()
  (autopilot

    yaw-left-button 
    yaw-right-button 
    pitch-up-button 
    pitch-down-button 
    roll-left-button 
    roll-right-button 

    translate-left-button 
    translate-right-button 
    translate-up-button 
    translate-down-button 
    translate-forward-button 
    translate-backward-button 

    reading-roll
    reading-roll-rate
    reading-pitch
    reading-pitch-rate
    reading-yaw
    reading-yaw-rate

    reading-translate-x
    reading-translate-y
    reading-translate-z
    reading-translate-rate
    )
  )

(defun main ()
  (let ((sim (init-sim)))
    (sleep 10) ; Wait for begin annimation
    (init-controllers sim)
    (ap)
    ))
;(main)


;(call-n-times (lambda () (click yaw-left-button)) 3)





















