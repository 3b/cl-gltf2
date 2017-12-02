(in-package :cl-user)

(defpackage #:cl-gltf2
  (:use #:cl
        #:alexandria
        #:parsley)
  (:export #:load-stream
           #:load-file))
