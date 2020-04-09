(in-package :rl)

(defstruct entity
  entity-type
  position)


(defstruct-type entity
  graphic
  color-fore)

;;(defun get-entity-graphic (entity)
  ;;(entity-type-graphic (entity-type entity)))
  
(defun setup-entity-types ()
  ;;(create-struct-type "entity")
  (put-entity-type 'player (make-entity-type :graphic #\@
					     :color-fore :white)))

(defun make-player-new ()
  (make-entity :entity-type (get-entity-type 'player)
	       :position '(2 . 3)))
