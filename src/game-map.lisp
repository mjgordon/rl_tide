(in-package :rl)

(defstruct game-map
  width
  height
  cells
  dmap)

(defstruct map-cell
  map-cell-type
  entity)

(defstruct-type map-cell
  graphic
  color-fore
  color-back
  passable)

(defun setup-cell-types ()
  (define-map-cell-type 'empty (make-map-cell-type :graphic #\.
						   :color-fore :white
						   :color-back :black
						   :passable t))
  (define-map-cell-type 'grass (make-map-cell-type :graphic #\Space
						   :color-fore :black
						   :color-back :green
						   :passable t))
  (define-map-cell-type 'wall (make-map-cell-type :graphic #\#
						  :color-fore :white
						  :color-back :black
						  :passable nil)))

(defun make-game-map-blank (w h)
  (let ((map (make-game-map :width w
			    :height h))
	(empty-type (lookup-map-cell-type 'grass)))
    (setf (game-map-cells map)
	  (let ((cells (make-array (game-map-size map))))
	    (loop for i from 0 below (game-map-size map) do
		 (setf (aref cells i) (make-map-cell :map-cell-type empty-type)))
	    cells))
    map))
				      
(defun make-game-map-test ()
  (let ((map (make-game-map-blank 60 60)))
    (mapgen-rect map 0 0 60 60 'wall)
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
  (if (rect-bounds-p x y (game-map-width map) (game-map-height map))
      (aref (game-map-cells map) (+ x (* y (game-map-width map))))
      nil))


(defun set-cell-type (map x y cell-type)
  (setf (map-cell-map-cell-type (get-cell map x y))
	(lookup-map-cell-type cell-type)))
		       

(defun move-entity-delta (map entity dx dy)
  "Moves an entity relatively to its current position"
  (let ((nx (+ (car (entity-position entity)) dx))
	(ny (+ (cdr (entity-position entity)) dy)))
    (move-entity map entity nx ny)))
	

(defun move-entity (map entity x y)
  "Moves an entity to a specific location"
  (let ((cell (get-cell map x y)))
    (if (map-cell-entity cell)
	(entity-attack entity (map-cell-entity cell))
	(when (and cell
		   (not (map-cell-entity cell))
		   (map-cell-passable cell))
	  (progn (setf (map-cell-entity (get-cell map (car (entity-position entity)) (cdr (entity-position entity)))) nil)
		 (setf (entity-position entity) (cons x y))
		 (setf (map-cell-entity (get-cell map x y)) entity))))))



(defun map-generate-dmap (game-map player)
  (let ((dmap (make-dmap-blank game-map)))
    (dmap-set-value dmap (entity-position player) 0)
    (setf (game-map-dmap game-map) (dmap-scan dmap game-map))))

