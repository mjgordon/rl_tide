(in-package :rl)

(defparameter *world* nil)
(defparameter *draw-dmap* t)

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

(defun world-add-entity (entity)
  "Finish setting up entity and properly adding it to the world"
  (setf (entity-hp entity) (entity-default-hp entity))
  (setf (world-entities *world*) (cons entity (world-entities *world*)))
  (setf (map-cell-entity (get-cell (world-current-map *world*)
				   (car (entity-position entity))
				   (cdr (entity-position entity))))
	entity))
			 

(defun redraw-world (world x y width height)
  "Redraws the current map, status messages, and player stats starting at (x,y), cropped to (width,height)"
  (redraw-map (world-current-map world) x y width height (world-offset world))
  (status-line-draw)
  (rl-graphics:set-string 0 0 (format nil "Timer : ~a" (read-timer)))
  (draw-player-stats (world-player world)))

(defun redraw-map (map x y width height offset)
  "Draws the current map starting at (x,y), cropped to (width,height)"
  (iterate-map map (lambda (map-x map-y id-x id-y)
		     (let* ((cell (get-cell map map-x map-y)))
		       (when cell
			 (let ((graphic (map-cell-graphic cell))
			       ;;(color-pair (list (map-cell-color-fore cell) (map-cell-color-back cell)))
			       (color-pair (list (map-cell-color-fore cell)
						 (rl-graphics::get-color-vec (rl-graphics::get-color-id (map-cell-color-back cell)))))
			       (entity (map-cell-entity cell)))
			   ;; Draws dmap bg instead. This is messy right now
			   (when *draw-dmap*
			     (let ((dmap-value (aref (game-map-dmap map) map-x map-y)))
			       (when (= dmap-value 9999)
				 (setf dmap-value 255))
			       (setf (second color-pair) (gamekit:vec4 (/ (- 255 dmap-value) 255.0)
								      0
								      (/ dmap-value 255.0)
								      1))))
			   (when entity
			     (setf graphic (entity-graphic entity))
			     (setf (first color-pair) (entity-color-fore entity)))
			   (rl-graphics:set-char id-x id-y graphic color-pair)))))
	       (+ x (car offset)) (+ y (cdr offset)) width height))

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
    (when (>= (car current-offset) (- (game-map-width map) w))
      (setf (car current-offset) (- (game-map-width map) w )))
    (when (>= (cdr current-offset) (- (game-map-height map) h))
      (setf (cdr current-offset) (- (game-map-height map) h )))))

(defun world-tick (world)
  (let ((dmap (map-generate-dmap (world-current-map world) (world-player world))))
    (mapc (lambda (entity)
	    (when (and (entity-alive entity)
		       (entity-ready-p entity))
	      (entity-update entity)))
	  (world-entities world))
    (inc-timer)))

(let ((timer 1))
  (defun read-timer ()
    timer)

  (defun reset-timer ()
    (setf timer 1))

  (defun inc-timer ()
    (incf timer)))


