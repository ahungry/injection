;;;; Container.lisp

(in-package #:injection)

(defparameter *container-singleton*
  "The latest instance of a 'Container object loaded through the container factory.")

(defclass Container ()
  ((Services
    :accessor Services
    :initarg :file-name
    :initform nil)
   (Parameters
    :accessor Parameters
    :initarg :parameters
    :initform nil)))

(defgeneric Container-Get-Service (Container name)
  (:documentation "Get a specific service NAME (class instance) from CONTAINER.SERVICES."))

(defgeneric Container-Get-Parameter (Container name)
  (:documentation "Get a specific parameter NAME from CONTAINER.PARAMETERS."))

(defgeneric Container-Load-File (Container file-name)
  (:documentation "Load the YAML 'FILE-NAME' into the CONTAINER.SERVICES and CONTAINER.PARAMETERS."))

(defmethod Container-Get-Service ((container Container) name)
  (gethash name (Services container)))

(defmethod Container-Get-Parameter ((container Container) name)
  (gethash name (Parameters container)))

(defmethod Container-Load-File ((container Container) file-name)
  (let ((file-loader (File-Loader-Factory file-name)))
    (when (Yaml file-loader)
      (setf (Parameters container) (gethash "parameters" (Yaml file-loader)))
      (setf (Services container) (gethash "services" (Yaml file-loader))))))

(defun Container-Factory (file-name &key (singleton nil))
  "Return an instance of CONTAINER class loaded up with FILE-NAME.
If SINGLETONE is t, also sets the *container-singleton* to the last
loaded file, so the shortcut functions can be used to directly access
the yml elements."
  (let ((container (make-instance 'Container)))
    (Container-Load-File container file-name)
    (when singleton (setf *container-singleton* container))
    container))

(defun get-service (name)
  "When the *container-singleton* is set, returns the service NAME."
  (when *container-singleton*
    (Container-Get-Service *container-singleton* name)))

(defun get-parameter (name)
  "When the *container-singleton* is set, returns the service NAME."
  (when *container-singleton*
    (Container-Get-Parameter *container-singleton* name)))

;;; "Container" goes here. Hacks and glory await!
