;;; xeh
;;; A command-line Canonical HEX+ASCII display tool written in ~40 lines of Common Lisp.
;;; https://github.com/obsfx/xeh

(defun slice (sequence start end &aux (l (length sequence)))
  (coerce (subseq sequence (min start l) (min end l)) 'list))

(defun hex-to-str-char (i)
  (if (< 31 i 127)
    (code-char i)
    #\.))

(defun dump-hex-row (buffer addr)
  (format t "~(~8,'0X  ~{~2,'0X~^ ~}  ~{~2,'0X~^ ~}~) ~60T|~A|~%"
          addr
          (slice buffer 0 8)
          (slice buffer 8 16)
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
  (format t "~(~8,'0X~)~%" (+ addr (length buffer))))

(defun main ()
  (sb-ext:disable-debugger)
  (dolist (path (or (rest *posix-argv*) '("-")))
    (if (string= path "-")
      (read-file-bytes *standard-input*)
      (with-open-file (stream path :element-type '(unsigned-byte 8))
        (read-file-bytes stream)))))
