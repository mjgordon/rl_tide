(in-package :rl)

(defparameter *game-state* nil)

(defstruct world-state
  current-map
  player
  entities
  offset)

(defun make-world-state-new ()
  (make-world-state :current-map (make-game-map-test)
		    :player (make-player-new)
		    :entities ()
		    :offset (cons 0 0)))

(defun get-direction-keys ()
  '(:0 :1 :2 :3 :4 :5 :6 :7 :8 :9
    :KEYPAD-1 :KEYPAD-2 :KEYPAD-3 :KEYPAD-4 :KEYPAD-5 :KEYPAD-6 :KEYPAD-7 :KEYPAD-8 :KEYPAD-9 :KEYPAD-0
    :UP :DOWN :LEFT :RIGHT))

(defun get-quit-keys ()
  '(#\q))

(defun get-event-specific (keys)
  (let ((event (get-event)))
    (if (member event keys) 
	event
	nil)))


(defun state-setup ()
  (setup-cell-types)
  (setup-entity-types)
  (setf *game-state* (make-world-state-new))
  (format t "~a~%" (world-state-offset *game-state*))
  (state-title-screen))

(defun state-title-screen ()
  (clear-screen)
  (let* ((title-text "LISP ROGUE")
	 (x (- (truncate *terminal-width* 2) (truncate (length title-text) 2)))
	 (y (truncate *terminal-height* 2)))
    (set-string x y title-text))
  #'state-game-normal)

(defun state-game-error ()
  (clear-screen)
  (let* ((title-text "ERROR")
	 (x (- (truncate *terminal-width* 2) (truncate (length title-text) 2)))
	 (y (truncate *terminal-height* 2)))
    (set-string x y title-text))
  #'state-game-error)


(defun state-game-normal ()
  (clear-screen)
  ;; We redraw the world last, but want to return the result of the state-switch
  (prog1
      (let ((event (get-event)))
	(when (member event (get-direction-keys))
	  (let ((delta nil))
	    (case event
	      ((:1 :KEYPAD-1) (setf delta '(-1 1)))
	      ((:2 :KEYPAD-2 :DOWN) (setf delta '(0 1)))
	      ((:3 :KEYPAD-3) (setf delta '(1 1)))
	      ((:4 :KEYPAD-4 :LEFT) (setf delta '(-1 0)))
	      ((:6 :KEYPAD-6 :RIGHT) (setf delta '(1 0)))
	      ((:7 :KEYPAD-7) (setf delta '(-1 -1)))
	      ((:8 :KEYPAD-8 :UP) (setf delta '(0 -1)))
	      ((:9 :KEYPAD-9) (setf delta '(1 -1))))
	    (when delta
	      (let ((map (world-state-current-map *game-state*))
		    (player (world-state-player *game-state*)))
	      (move-entity-delta map player (first delta) (second delta))
	      (map-adjust-offset map (entity-position player) 20 20)))))
	
	#'state-game-normal)
    (redraw-world-state *game-state* 0 0 20 20)))

  
(defun redraw-world-state (state x y width height)
  "Redraws the current map, starting at (x,y), cropped to (width,height)"
  (let* ((map (world-state-current-map state))
	 (offset (world-state-offset *game-state*)))
    (iterate-map map (lambda (map-x map-y id-x id-y)
		       (let ((cell (get-cell map map-x map-y)))
			 (when cell
			   (set-char id-x id-y (map-cell-graphic cell)))))
		 (+ x (car offset)) (+ y (cdr offset)) width height)
    (draw-entity (world-state-player state))
    (status-line-draw)))
    

		 
		 
(defun draw-entity (entity)
  (let ((x (- (car (entity-position entity)) (car (world-state-offset *game-state*)) ))
	(y (- (cdr (entity-position entity)) (cdr (world-state-offset *game-state*)) )))
    (set-char x y (entity-graphic entity))))
    

(defun map-adjust-offset (map pos w h &optional (gutter 5))
  "Set the map offset for subwindow size (w,h) such that pos remains within gutter of the edges"
  (let* ((current-offset (world-state-offset *game-state*))
	 (adj-x (- (car pos) (car current-offset)))
	 (adj-y (- (cdr pos) (cdr current-offset))))
    ;; Adjust for position
    (when (> adj-x (- w gutter))
      (incf (car current-offset) (- adj-x (- w gutter))))
    (when (< adj-x gutter)
      (decf (car current-offset) (- gutter adj-x)))

    (when (> adj-y (- h gutter))
      (incf (cdr current-offset) (- adj-y (- h gutter))))
    (when (< adj-y gutter)
      (decf (cdr current-offset) (- gutter adj-y)))

    ;; Clip to map edges
    (when (< (car current-offset) 0)
      (setf (car current-offset) 0))
    (when (< (cdr current-offset) 0)
      (setf (cdr current-offset) 0))
    (when (>= (car current-offset) (- (game-map-width map) w 1))
      (setf (car current-offset) (- (game-map-width map) w 1)))
    (when (>= (cdr current-offset) (- (game-map-height map) h 1))
      (setf (cdr current-offset) (- (game-map-height map) h 1)))))
