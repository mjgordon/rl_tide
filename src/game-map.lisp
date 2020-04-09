(in-package :rl)

(defstruct game-map
  width
  height
  cells)

(defstruct map-cell
  map-cell-type
  entities)

(defstruct-type map-cell
  graphic
  color-fore
  color-back
  passable)

(defun setup-cell-types ()
  (put-map-cell-type 'empty (make-map-cell-type :graphic #\.
						:color-fore :white
						:color-back :black
						:passable T))
  (put-map-cell-type 'wall (make-map-cell-type :graphic #\#
					       :color-fore :white
					       :color-back :black
					       :passable nil)))
				      
(defun make-game-map-test ()
  (let ((map (make-game-map :width 10
			    :height 10)))
    (setf (game-map-cells map) (make-list (game-map-size map)
					  :initial-element (make-map-cell :map-cell-type (get-map-cell-type 'empty))))
    map))


(defun game-map-size (map)
  (* (game-map-width map) (game-map-height map)))

(defmacro iterate-map (map &rest body)
  `(let ((map-width (game-map-width ,map))
	 (map-height (game-map-height ,map)))
     (loop for cell in (game-map-cells ,map)
	for i from 0 do
	  (let ((x (mod i map-width))
		(y (truncate i map-height)))
	    ,@body))))

(defun get-cell (map x y)
  (if (or (< x 0) (< y 0) (>= x (game-map-width map)) (>= y (game-map-height map)))
      nil
      (nth (+ x (* y (game-map-width map))) (game-map-cells map))))

(defun move-entity-delta (map entity dx dy)
  (let ((nx (+ (car (entity-position entity)) dx))
	(ny (+ (cdr (entity-position entity)) dy)))
    (move-entity map entity nx ny)))
	

(defun move-entity (map entity x y)
  (let ((cell (get-cell map x y)))
    (if (and cell
	     (not (map-cell-entities cell))
	     (map-cell-passable cell))
	(progn (setf (entity-position entity) (cons x y))
	       (format nil "Moved to ~a ~a ~%" x y))
	(format nil "Can't move there bro~%"))))
