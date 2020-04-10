(in-package :rl)

(defparameter *world* nil)

(defstruct world
  current-map
  player
  entities
  offset)

(defun make-world-new ()
  (make-world :current-map (make-game-map-test)
	      :player (make-player-new)
	      :entities ()
	      :offset (cons 0 0)))

(defun redraw-world (world x y width height)
  "Redraws the current map, starting at (x,y), cropped to (width,height)"
  (let* ((map (world-current-map world))
	 (offset (world-offset world)))
    (iterate-map map (lambda (map-x map-y id-x id-y)
		       (let* ((cell (get-cell map map-x map-y)))
			 (when cell
			   (let ((graphic (map-cell-graphic cell))
				 (color-pair (list (map-cell-color-fore cell) (map-cell-color-back cell)))
				 (entity (map-cell-entity cell)))
			     (when entity
			       (setf graphic (entity-graphic entity))
			       (setf (first color-pair) (entity-color-fore entity)))
			     (set-char id-x id-y graphic color-pair)))))
		 (+ x (car offset)) (+ y (cdr offset)) width height)
    ;;(draw-entity (world-player world))
    (status-line-draw)))

(defun world-adjust-offset (world pos w h &optional (gutter 5))
  "Set the map offset for subwindow size (w,h) such that pos remains within gutter of the edges"
  (let* ((map (world-current-map world))
	 (current-offset (world-offset world))
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
