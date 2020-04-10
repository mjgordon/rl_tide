(in-package :rl)

;; Maybe this should be a ring buffer
;; For now we'll just occasionally chop off the end. Works but dumb?
(let ((status-list ()))
  
  (defun status-line-add (line)
    "Adds a new line of text to the message list"
    (setf status-list (cons (write-to-string line) status-list))
    (when (> (length status-list) 30)
      (setf (cdr (nthcdr 19 status-list)) nil)))
  
  (defun status-line-clear ()
    "Clears the message list"
    (setf status-list ()))

  (defun status-line-draw (&optional (x 1) (count 10))
    "Draws the message list to the screen"
    (unless (equal (length status-list) 0)
      (when (< (length status-list) count)
	(setf count (length status-list)))
      (loop for ypos from (- *terminal-height* 1) downto (- *terminal-height* count) and id from 0 do
	   (set-string x ypos (nth id status-list)))))
    
)
