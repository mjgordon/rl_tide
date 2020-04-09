(in-package :rl)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defparameter *char-width* 8)
(defparameter *char-height* 12)

(defparameter *terminal-width* nil)
(defparameter *terminal-height* nil)


(defparameter *char-data* nil)
(defparameter *fg-data* nil)
(defparameter *bg-data* nil)

(defparameter *redraw-flag* t)

(defun get-color-id (color)
  (cond ((equal color :black) 0)
	((equal color :red) 1)
	((equal color :green) 2)
	((equal color :yellow) 3)
	((equal color :blue) 4)
	((equal color :magenta) 5)
	((equal color :cyan) 6)
	((equal color :white) 7)))

(defun get-spritesheet-name (color)
  (cond ((equal color 0) :curses-black)
	((equal color 1) :curses-red)
	((equal color 2) :curses-green)
	((equal color 3) :curses-yellow)
	((equal color 4) :curses-blue)
	((equal color 5) :curses-magenta)
	((equal color 6) :curses-cyan)
	((equal color 7) :curses-white)))

(defun get-color-vec (color)
  (cond ((equal color 0) (gamekit:vec4 0 0 0 1))
	((equal color 1) (gamekit:vec4 1 0 0 1))
	((equal color 2) (gamekit:vec4 0 1 0 1))
	((equal color 3) (gamekit:vec4 1 1 0 1))
	((equal color 4) (gamekit:vec4 0 0 1 1))
	((equal color 5) (gamekit:vec4 1 0 1 1))
	((equal color 6) (gamekit:vec4 0 1 1 1))
	((equal color 7) (gamekit:vec4 1 1 1 1))))

(defun setup-terminal ()
  "Called at startup and after window resizing. Initializes the arrays that hold terminal data"
  (setf *terminal-width* (floor (/ *window-width* *char-width*)))
  (setf *terminal-height* (floor (/ *window-height* *char-height*)))
  (format t "Terminal Size : ~a : ~a~%" *terminal-width* *terminal-height*)
  (clear-screen)
  (demo))

(defun demo ()
  (randomize)
  (set-string 0 0 "First Line")
  (set-string 1 1 "Second Line")
  (set-string (- *terminal-width* (length "Last Line")) (- *terminal-height* 1) "Last Line")
  (set-char 3 3 #\a)
  (fill-bg :blue))

(defun randomize ()
  (loop for id from 0 to (- (length *char-data*) 1) do
       (setf (aref *char-data* id) (random 256))
       (setf (aref *fg-data* id) (random 8))
       (setf (aref *bg-data* id) (random 8))))

(defun randomize-bg ()
  (loop for id from 0 to (- (length *bg-data*) 1) do
       (setf (aref *bg-data* id) (random 8))))

(defun fill-bg (color)
  (let ((color-id (get-color-id color)))
    (loop for id from 0 to (- (length *bg-data*) 1) do
	 (setf (aref *bg-data* id) color-id))))

(defun cart-to-loc (x y)
  (+ x (* y *terminal-width*)))
    
(defun get-direction-keys ()
  '(:0 :1 :2 :3 :4 :5 :6 :7 :8 :9
    :KEYPAD-1 :KEYPAD-2 :KEYPAD-3 :KEYPAD-4 :KEYPAD-5 :KEYPAD-6 :KEYPAD-7 :KEYPAD-8 :KEYPAD-9 :KEYPAD-0
    :UP :DOWN :LEFT :RIGHT))

(defun get-quit-keys ()
  '(#\q))

(defun set-char (x y c &optional (color-pair '(:white :black)))
  (let ((cell-id (cart-to-loc x y)))
    (setf (aref *char-data* cell-id) (char-int c))
    (setf (aref *fg-data* cell-id) (get-color-id (first color-pair)))
    (setf (aref *bg-data* cell-id) (get-color-id (second color-pair)))))

(defun set-string (x y string &optional (color-pair '(:white :black)))
  (loop for c across string and idx from 0 do
       (set-char (+ x idx) y c color-pair)))

(defun clear-screen()
  (let ((data-length (* *terminal-width* *terminal-height*)))
    (setf *char-data* (make-array data-length :initial-element 0))
    (setf *fg-data* (make-array data-length :initial-element (get-color-id :white)))
    (setf *bg-data* (make-array data-length :initial-element (get-color-id :black)))))
  

(defun redraw()
  (loop for y from 0 to (- *terminal-height* 1) do 
       (loop for x from 0 to (- *terminal-width* 1) do
	    (let* ((loc (cart-to-loc x y))
		   (cart-pos (gamekit:vec2 (* x *char-width*) (- *window-height* (* (+ y 1) *char-height*))))
		   (c (aref *char-data* loc))
		   (origin (gamekit:vec2 (* *char-width* (mod c 16)) (- 192 (* *char-height* (+ 1(floor (/ c 16)))))))
		   (fg (aref *fg-data* loc))
		   (bg (aref *bg-data* loc)))
	      (gamekit:draw-rect cart-pos *char-width* *char-height* :fill-paint (get-color-vec bg))
	      (gamekit:draw-image cart-pos
				  (get-spritesheet-name fg)
				  :origin origin
				  :width *char-width*
				  :height *char-height*)))))





  
