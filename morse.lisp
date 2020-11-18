;;;; morse.lisp
;;;; Translates from morse code to strings, back and forth.
;;;; My homework while learning Marco Baringer's SLIME Tutorial Video.

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :morse
  (:use :common-lisp)
  (:export char-to-morse morse-to-char word-to-morse morse-to-word text-to-morse morse-to-text))

(in-package :morse)

(defparameter *morse-mapping*
  '((#\A ".-")
    (#\B "-...")
    (#\C "-.-.")
    (#\D "-..")
    (#\E ".")
    (#\F "..-.")
    (#\G "--.")
    (#\H "....")
    (#\I "..")
    (#\J ".---")
    (#\K "-.-")
    (#\L ".-..")
    (#\M "--")
    (#\N "-.")
    (#\O "---")
    (#\P ".--.")
    (#\Q "--.-")
    (#\R ".-.")
    (#\S "...")
    (#\T "-")
    (#\U "..-")
    (#\V "...-")
    (#\W ".--")
    (#\X "-..-")
    (#\Y "-.--")
    (#\Z "--..")
    (#\0 "-----")
    (#\1 ".----")
    (#\2 "..---")
    (#\3 "...--")
    (#\4 "....-")
    (#\5 ".....")
    (#\6 "-....")
    (#\7 "--...")
    (#\8 "---..")
    (#\9 "----.")
    (#\. ".-.-.-")
    (#\, "--..--")
    (#\? "..--..")
    (#\! "-.-.--")
    (#\' ".----.")))

(defun char-to-morse (char)
  (second (assoc char *morse-mapping* :test #'char-equal)))

(defun morse-to-char (morse-char)
  (first (rassoc morse-char *morse-mapping* :test #'string= :key #'car)))

(defun word-to-morse (word)
  (let ((morse-word (with-output-to-string (morse-word-stream)
		      (loop
			for char across word
			do (write-string (char-to-morse char) morse-word-stream)
			do (write-char #\Space morse-word-stream)))))
    (string-right-trim " " morse-word)))

(defun morse-to-word (morse-word)
  (with-output-to-string (word-stream)
    (loop
      for morse-char in (uiop:split-string morse-word)
      do (write-char (morse-to-char morse-char) word-stream))))

(defun text-to-morse (text)
  (let ((str (with-output-to-string (morse-text-stream)
	       (loop
		 for word in (uiop:split-string text)
		 do (write-string (word-to-morse word) morse-text-stream)
		 do (write-string "  " morse-text-stream)))))
    (string-right-trim " " str)))

(defun morse-to-text (morse)
  (with-output-to-string (text-stream)
    (loop
      for morse-char in (uiop:split-string morse)
      for char = (if (string/= morse-char "") (morse-to-char morse-char) #\Space)
      do (write-char char text-stream))))

