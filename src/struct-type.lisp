(in-package :rl)

#|
Provides a transparent way to cheaply associate static 'type' data with a struct, e.g. the graphics properties
of a map cell or default stats of a mob

Usage:
After calling (defstruct mystruct), call (defstruct-type mystruct) with the static data items listed normally. 
Accessors will be transparently created as though they were part of the normal struct. 
Types can be created with (define-mystruct-type 'name (make-mystruct-type etc...))
and accessed with (lookup-mystruct-type 'name)
|#

(defmacro defstruct-type (parent-type &rest body)
  (let ((type-name (concatenate-symbols parent-type 'type))
	(map-name (concatenate-symbols parent-type 'types))
	(put-name (intern (string-upcase (format nil "define-~a-type" parent-type))))
	(get-name (intern (string-upcase (format nil "lookup-~a-type" parent-type)))))
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
