(in-package :cl-gltf2)

(defclass datastream ()
  ((%header :reader header)
   (%chunks :reader chunks)))

(defclass header ()
  ((%magic :reader format-magic)
   (%version :reader format-version)
   (%length :reader format-length)))

(defun parse-header ()
  (let ((header (make-instance 'header)))
    (with-slots (%magic %version %length) header
      (with-buffer-read (:sequence (read-bytes 12))
        (let ((magic (read-string :bytes 4)))
          (if (not (string= magic "glTF"))
              (error "Invalid glTF2 file.")
              (setf %magic magic
                    %version (read-uint-le 4)
                    %length (read-uint-le 4))))))
    header))

(defun end-of-stream-p ()
  (let ((stream (buffer-stream)))
    (= (file-position stream)
       (file-length stream))))

(defun parse-chunks ()
  (loop :until (end-of-stream-p)
        :for chunk = (parse-chunk)
        :collect chunk))

(defun parse-datastream ()
  (let ((datastream (make-instance 'datastream)))
    (with-slots (%header %chunks) datastream
      (setf %header (parse-header)
            %chunks (parse-chunks)))
    datastream))
