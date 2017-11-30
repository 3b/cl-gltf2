(in-package :cl-gltf2)

(defvar *object*)

(deftype ub8 () '(unsigned-byte 8))

(defclass gltf2 ()
  ((parse-tree :accessor parse-tree)))

(defun load-stream (stream)
  (with-buffer-read (:stream stream)
    (let ((*object* (make-instance 'gltf2)))
      (setf (parse-tree *object*) (parse-datastream))
      *object*)))

(defun load-file (path)
  (with-open-file (in path :element-type 'ub8)
    (load-stream in)))
