(in-package :rl-graphics)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defparameter *char-width* 8)
(defparameter *char-height* 12)

(defparameter *terminal-width* nil)
(defparameter *terminal-height* nil)

;; Array of ascii ids into code page 437
(defparameter *char-data* nil)

;; Array of foreground color ids. (0-8) for black, red, green etc...
(defparameter *fg-data* nil)

;; Array of background color vectors. For flexibility and debugging (e.g. displaying dmaps)
;;these are stored as raw colors instead of ids. 
(defparameter *bg-data* nil)

;; Part of a dirty dirty hack. See handle-drawing in main.lisp
(defparameter *redraw-flag* 0)


(defun setup-terminal ()
  "Called at startup and after window resizing. Initializes the arrays that hold terminal data"
  (setf *terminal-width* (floor (/ *window-width* *char-width*)))
  (setf *terminal-height* (floor (/ *window-height* *char-height*)))
  ;;(format t "Terminal Size : ~a : ~a~%" *terminal-width* *terminal-height*)
  (clear-screen)
  (demo))


(defun set-char (x y c &optional (color-pair '(:white :black)))
  (unless (or (>= x *terminal-width*)
	      (>= y *terminal-height*)
	      (< x 0)
	      (< y 0))
    (let ((cell-id (rl::cart-to-loc x y)))
      (setf (aref *char-data* cell-id) (char-int c))
      (setf (aref *fg-data* cell-id) (get-color-id (first color-pair)))
      ;;(setf (aref *bg-data* cell-id) (get-color-id (second color-pair)))
      (setf (aref *bg-data* cell-id) (second color-pair)))))


(defun set-string (x y string &optional (color-pair (list :white (get-color-vec (get-color-id :black)))))
  (loop for c across string and idx from 0 do
       (set-char (+ x idx) y c color-pair)))


(defun clear-screen()
  (let ((data-length (* *terminal-width* *terminal-height*)))
    (setf *char-data* (make-array data-length :initial-element 0))
    (setf *fg-data* (make-array data-length :initial-element (get-color-id :white)))
    (setf *bg-data* (make-array data-length :initial-element (get-color-vec (get-color-id :black))))))
  

(defun redraw()
  (loop for y from 0 to (- *terminal-height* 1) do 
       (loop for x from 0 to (- *terminal-width* 1) do
	    (let* ((loc (rl::cart-to-loc x y))
		   (cart-pos (gamekit:vec2 (* x *char-width*)
					   (- *window-height* (* (+ y 1) *char-height*))))
		   (c (aref *char-data* loc))
		   (origin (gamekit:vec2 (* *char-width* (mod c 16))
					 (- 192 (* *char-height* (+ 1 (floor (/ c 16)))))))
		   (fg (aref *fg-data* loc))
		   (bg (aref *bg-data* loc)))
	      (unless (equal bg 4)
		;;(gamekit:draw-rect cart-pos *char-width* *char-height* :fill-paint (get-color-vec bg)))
		(gamekit:draw-rect cart-pos *char-width* *char-height* :fill-paint bg))
	      (unless (or (equal c 0) (equal c 32))
		(gamekit:draw-image cart-pos
				    (get-spritesheet-name fg)
				    :origin origin
				    :width *char-width*
				    :height *char-height*))))))


;;; LOOKUPS AND UTILITY

(defun get-color-id (color)
  (cond ((equal color :black) 0)
	((equal color :red) 1)
	((equal color :green) 2)
	((equal color :yellow) 3)
	((equal color :blue) 4)
	((equal color :magenta) 5)
	((equal color :cyan) 6)
	((equal color :white) 7)))

(let ((spritesheet-names (list :curses-black
			       :curses-red
			       :curses-green
			       :curses-yellow
			       :curses-blue
			       :curses-magenta
			       :curses-cyan
			       :curses-white)))
  (defun get-spritesheet-name (id)
    (nth id spritesheet-names)))

(let ((color-vecs (list (gamekit:vec4 0 0 0 1)
			(gamekit:vec4 0.5 0 0 1)
			(gamekit:vec4 0 0.5 0 1)
			(gamekit:vec4 0.5 0.5 0 1)
			(gamekit:vec4 0 0 0.5 1)
			(gamekit:vec4 0.5 0 0.5 1)
			(gamekit:vec4 0 0.5 0.5 1)
			(gamekit:vec4 0.5 0.5 0.5 1))))
  (defun get-color-vec (id)
    (nth id color-vecs)))

;;; DEMO STUFF

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



  

