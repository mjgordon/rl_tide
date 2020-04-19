(in-package :rl)

(defun make-dmap-blank (game-map)
  (let ((dmap (make-array (list (game-map-width game-map) (game-map-height game-map))
			  :initial-element 99999)))
    dmap))

(defun dmap-set-value (dmap pos value)
  (setf (aref dmap (car pos) (cdr pos)) value))

(defun dmap-get-neighbor-values (dmap x y)
  (let ((output '())
	(dmap-width (first (array-dimensions dmap)))
	(dmap-height (second (array-dimensions dmap))))
    (loop for n from 0 below 8 do
	 (let ((nx (+ x (nth n *dx*)))
	       (ny (+ y (nth n *dy*))))
	   (when (rect-bounds-p nx ny dmap-width dmap-height)
	     (setf output (cons (aref dmap nx ny)
				output)))))
    output))
  

(defun dmap-scan (dmap game-map &optional (recurse t) (iterations 0))
  (let ((output (make-array (array-dimensions dmap)
			    :element-type (array-element-type dmap)
			    :initial-element 9999))
	(changes 0))
    (destructuring-bind (x-size y-size) (array-dimensions dmap)
      (loop for y from 0 below y-size do
	   (loop for x from 0 below x-size do
		(let ((cell (get-cell game-map x y)))
		  (when (map-cell-passable cell)
		    (let* ((neighbors (dmap-get-neighbor-values dmap x y))
			   (least-neighbor (apply #'min neighbors))
			   (value (aref dmap x y)))
		      (if (> value (+ 1 least-neighbor))
			  (progn
			    (incf changes)
			    (setf (aref output x y) (+ 1 least-neighbor)))
			  (setf (aref output x y) value))))))))
    (if (and recurse (> changes 0) (< iterations 1000))
	(progn
	  ;;(format t "changes : ~a~%" changes)
	  (dmap-scan output game-map t (+ iterations 1)))
	(progn
	  (print-hash-table (array-set-counts output))
	  ;;(format t "Iterations : ~a~%" iterations)
	  output))))

		    
		    
		
    
    
	
	  
    
	
