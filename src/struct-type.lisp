(in-package :rl)

(defmacro defstruct-type (parent-type &rest body)
  (let ((type-name (concatenate-symbols parent-type 'type))
	(map-name (concatenate-symbols parent-type 'types))
	(put-name (intern (string-upcase (format nil "put-~a-type" parent-type))))
	(get-name (intern (string-upcase (format nil "get-~a-type" parent-type)))))
    `(progn (defstruct ,type-name
	      ,@body)
	    ,@(mapcar (lambda (slot)
			`(defstruct-type-function ,parent-type ,type-name ,slot))
		      body)
	    (let ((,map-name (make-hash-table)))
	      (defun ,put-name (name content)
		(setf (gethash name ,map-name) content))
	      (defun ,get-name (name)
		(gethash name ,map-name))))))

(defmacro defstruct-type-function (parent-type struct-name slot)
  `(defun ,(concatenate-symbols parent-type slot) (type)
     (,(concatenate-symbols struct-name slot) (,(concatenate-symbols parent-type struct-name) type))))


(defmacro create-struct-type (name)
  (let ((map-name (intern (string-upcase (concatenate 'string name "-types"))))
	(put-name (intern (string-upcase (format nil "put-~a-type" name))))
	(get-name (intern (string-upcase (format nil "get-~a-type" name)))))
    `(let ((,map-name (make-hash-table)))
       (defun ,put-name (name content)
	 (setf (gethash name ,map-name) content))
       (defun ,get-name (name)
	 (gethash name ,map-name)))))
	
