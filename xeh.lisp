;;; xeh
;;; A command-line Canonical HEX+ASCII display tool written in ~80 lines of Common Lisp.
;;; based on https://github.com/obsfx/xeh

(defparameter *color* nil)

(defun rgb (r g b)
  (+ 16 (* 36 r) (* 6 g) (* 1 b)))

(defmacro esc (format-string &rest args)
  `(format nil ,(concatenate 'string "~A" format-string)
     #\escape ,@args))

(defun ansi-reset ()
  (esc "[0m"))

(defparameter *colors*
  (vector (rgb 5 2 1) (rgb 5 1 2) ;; control chars
          (rgb 3 1 5) (rgb 4 2 5) ;; symbols and numbers
          (rgb 1 3 5) (rgb 2 4 5) ;; uppercase
          (rgb 1 1 5) (rgb 2 2 5) ;; lowercase
          (rgb 3 5 3) (rgb 3 5 1) ;; high 10
          (rgb 2 5 0) (rgb 1 5 0)
          (rgb 5 5 3) (rgb 5 5 2) ;; high 11
          (rgb 5 5 1) (rgb 5 5 0)))

(defun byte-color (byte)
  (esc "[38;5;~Dm" (case byte
                     (#x00 (rgb 1 1 1))
                     (#xFF (rgb 5 5 5))
                     (#x0A (rgb 5 3 0)) ; lf
                     (#x0D (rgb 5 0 0)) ; cr
                     (#x09 (rgb 5 2 1)) ; tab
                     (t (aref *colors* (ldb (byte 4 4) byte))))))

(defun byte-to-str (byte &aux (str (format nil "~2,'0X" byte)))
  (if *color*
    (format nil "~A~A~A" (byte-color byte) str (ansi-reset))
    str))

(defun slice (sequence start end &aux (l (length sequence)))
  (coerce (subseq sequence (min start l) (min end l)) 'list))

(defun hex-to-str-char (i)
  (if (< 31 i 127)
    (code-char i)
    #\.))

(defun dump-hex-row (buffer addr)
  (format t "~8,'0X  ~{~A~^ ~}  ~{~A~^ ~} ~vA|~A|~%"
          addr
          (mapcar #'byte-to-str (slice buffer 0 8))
          (mapcar #'byte-to-str (slice buffer 8 16))
          (+ 1 (* 3 (- 16 (length buffer))))
          ""
          (map 'string #'hex-to-str-char buffer)))

(defun read-file-bytes (stream &aux (addr 0) (buffer (make-array 16 :fill-pointer 0)))
  (loop :for byte = (read-byte stream nil nil)
        :while byte
        :do (progn (vector-push byte buffer)
                   (when (= (length buffer) 16)
                     (dump-hex-row buffer addr)
                     (setf (fill-pointer buffer) 0
                           addr (+ addr 16)))))
  (when (plusp (length buffer))
    (dump-hex-row buffer addr))
  (format t "~8,'0X~%" (+ addr (length buffer))))

(defun main (&aux (args (rest *posix-argv*)))
  (sb-ext:disable-debugger)
  (when (member "--color" args :test #'string=)
    (setf *color* t args (remove "--color" args :test #'string=)))
  (handler-case
      (dolist (path (or args '("-")))
        (if (string= path "-")
          (read-file-bytes *standard-input*)
          (with-open-file (stream path :element-type '(unsigned-byte 8))
            (read-file-bytes stream))))
    (sb-int:broken-pipe () (return-from main))))
