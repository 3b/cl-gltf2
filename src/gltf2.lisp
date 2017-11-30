(in-package :cl-gltf2)

(defvar *object*)

(defclass gltf2 ()
  ((parse-tree :accessor parse-tree)
   (json :accessor json)))

(defun load-stream (stream)
  (with-buffer-read (:stream stream)
    (let ((*object* (make-instance 'gltf2)))
      (setf (parse-tree *object*) (parse-datastream))
      *object*)))

(defun load-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (load-stream in)))
