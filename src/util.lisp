(in-package :rl)



(defun trace-dummy ())

(defun get-stack-depth()
  (trace trace-dummy)
  (trace-dummy)
  (untrace trace-dummy))

(defun concatenate-symbols (a b)
  (intern (concatenate 'string (symbol-name a) "-" (symbol-name b))))
