;;;; File-Loader.lisp

(in-package #:injection)

(defclass File-Loader ()
  ((Name
    :accessor File-Name
    :initarg :file-name
    :initform "")
   (Content
    :accessor Content
    :initarg :content
    :initform nil)
   (Yaml
    :accessor Yaml
    :initarg :yaml
    :initform nil)))

(defgeneric Load-Content (File-Loader)
  (:documentation "Load FILE-LOADER.CONTENT based on FILE-LOADER.FILE-NAME."))

(defmethod Load-Content ((file File-Loader))
  (setf (Content file)
        (with-open-file
            (stream (File-Name file)
                    :direction :input
                    :if-does-not-exist :error)
          (when stream
            (loop for line = (read-line stream nil 'eof)
               until (eq line 'eof)
               collect line)))))

(defgeneric Parse-Yaml (File-Loader)
  (:documentation "Parse FILE-LOADER.CONTENT into FILE-LOADER.YAML."))

(defmethod Parse-Yaml ((file File-Loader))
  (setf (Yaml file)
        (yaml:parse (format nil "~{~a~%~}" (Content file)))))

(defun File-Loader-Factory (file-name)
  "Return an initialized FILE-LOADER class loaded up with FILE-NAME."
  (let ((file-loader (make-instance 'File-Loader :file-name file-name)))
    (Load-Content file-loader)
    (Parse-Yaml file-loader)
    file-loader))

;;; "File-Loader" goes here. Hacks and glory await!
