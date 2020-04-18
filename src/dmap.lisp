(in-package :rl)

(defun make-dmap-blank (game-map)
  (let ((l (game-map-size game-map))
	(dmap (make-array '((game-map-width game-map) (game-map-height game-map)) :initial-element 99999)))
    dmap))
    
	
	  
    
	
