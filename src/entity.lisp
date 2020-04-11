(in-package :rl)

(defstruct entity
  entity-type
  position
  (behavior 'random)
  (speed 10))

(defstruct-type entity
  graphic
  color-fore)

  
(defun setup-entity-types ()
  (define-entity-type 'player (make-entity-type :graphic #\@
						:color-fore :white))
  (define-entity-type 'orc (make-entity-type :graphic #\o
					     :color-fore :white)))

(defun make-player-new ()
  (make-entity :entity-type (lookup-entity-type 'player)
	       :position (cons 2 3)))


(defun entity-update (entity)
  (case (entity-behavior entity)
    ('random (entity-move-random entity))))

(defun entity-move-random (entity)
  (let* ((dir (random 8))
	 (dx (nth dir *dx*))
	 (dy (nth dir *dy*))
	 (map (world-current-map *world*)))
    (move-entity-delta map entity dx dy)))
	   
