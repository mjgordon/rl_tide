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
  (define-map-cell-type 'empty (make-map-cell-type :graphic #\.
						   :color-fore :white
						   :color-back :black
						   :passable T))
  (define-map-cell-type 'wall (make-map-cell-type :graphic #\#
						  :color-fore :white
						  :color-back :black
						  :passable nil)))

(defun make-game-map-blank (w h)
  (let ((map (make-game-map :width w
			    :height h))
	(empty-type (lookup-map-cell-type 'empty)))
    (setf (game-map-cells map)
	  (loop repeat (game-map-size map)
	       collect (make-map-cell :map-cell-type empty-type)))
    map))
				      
(defun make-game-map-test ()
  (let ((map (make-game-map-blank 200 200)))
    (mapgen-rect map 0 0 200 200 'wall)
    map))
  

(defun game-map-size (map)
  (* (game-map-width map) (game-map-height map)))

;; Might be interesting to do map cropping with n-dimensional slicing.
;; For now we use for loops :D
(defun iterate-map (map fun &optional (x 0) (y 0) (w -1) (h -1))
  "Loops over cells in map starting from (x,y), in dimensions (w,h). 
Calls function 'fun' with arguments (map-x map-y x-id y-id)"
  ;; Set the dimensions to the whole map if not provided
  (let ((crop-width (if (= w -1) (game-map-width map) w))
	(crop-height (if (= h -1) (game-map-height map) h)))
     (loop for map-y from y to (+ y crop-height) and y-id from 0 do
	  (loop for map-x from x to (+ x crop-width) and x-id from 0 do
	       (funcall fun map-x map-y x-id y-id)))))
			 

(defun get-cell (map x y)
  (if (or (< x 0) (< y 0) (>= x (game-map-width map)) (>= y (game-map-height map)))
      nil
      (nth (+ x (* y (game-map-width map))) (game-map-cells map))))

(defun set-cell-type (map x y cell-type)
  (setf (map-cell-map-cell-type (get-cell map x y)) (lookup-map-cell-type cell-type)))
		       

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
