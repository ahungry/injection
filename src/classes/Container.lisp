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
    :initform nil)
   (Instances
    :accessor Instances
    :initarg :instances
    :initform (make-hash-table :test #'equal))))

(defgeneric Container-Get-Service (Container name)
  (:documentation "Get a specific service NAME (class instance) from CONTAINER.SERVICES."))

(defgeneric Container-Get-Parameter (Container name)
  (:documentation "Get a specific parameter NAME from CONTAINER.PARAMETERS."))

(defgeneric Container-Load-File (Container file-name)
  (:documentation "Load the YAML 'FILE-NAME' into the CONTAINER.SERVICES and CONTAINER.PARAMETERS."))

(defgeneric Container-Expand-Services (Container arguments)
  (:documentation "Look for arguments with the '@' symbol and expand."))

(defgeneric Container-Instantiate-Services (Container)
  (:documentation "Instantiate each service (including dependent ones)."))

(defmethod Container-Get-Service ((container Container) name)
  (when (typep (Instances container) 'hash-table)
    (gethash name (Instances container))))

(defmethod Container-Get-Parameter ((container Container) name)
  (when (typep (Parameters container) 'hash-table)
    (gethash name (Parameters container))))

(defmethod Container-Load-File ((container Container) file-name)
  (let ((file-loader (File-Loader-Factory file-name)))
    (when (Yaml file-loader)
      (setf (Parameters container) (gethash "parameters" (Yaml file-loader)))
      (setf (Services container) (gethash "services" (Yaml file-loader))))))

(defmethod Container-Expand-Services ((container Container) arguments)
  "If we have any arguments that begin with an '@' symbol, we want to expand
into the equivalent call to (Container-Get-Service container name)."
  (mapcar (lambda (arg)
            (cond
              ;; A service was found via @service_name
              ((string= "@" arg :end2 1)
               (Container-Get-Service container (subseq arg 1)))

              ;; A parameter was found via %parameter_name%
              ((and (string= "%" arg :end2 1)
                    (string= "%" arg :start2 (1- (length arg))))
               (Container-Get-Parameter container (subseq arg 1 (1- (length arg)))))

              (t arg)))
          arguments))

(defmethod Container-Instantiate-Services ((container Container))
  (when (typep (Services container) 'hash-table)
    (loop for key being the hash-keys of (Services container)
       using (hash-value value)
       do (progn
            (unless (gethash "factory" value) (error "Missing key 'factory' in YML file."))
            (let ((arguments (gethash "arguments" value)))
              (setf arguments (Container-Expand-Services container arguments))
              (print arguments)
              (setf (gethash key (Instances container))
                    (apply (intern (string-upcase (gethash "factory" value)))
                           arguments)))))))

(defun Container-Factory (file-name &key (singleton nil))
  "Return an instance of CONTAINER class loaded up with FILE-NAME.
If SINGLETON is t, also sets the *container-singleton* to the last
loaded file, so the shortcut functions can be used to directly access
the yml elements."
  (let ((container (make-instance 'Container)))
    (unless (stringp file-name) (print (File-Name file-name)) (setf file-name (File-Name file-name)))
    (Container-Load-File container file-name)
    (Container-Instantiate-Services container)
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
