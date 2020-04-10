(in-package :rl)

(defstruct entity
  entity-type
  position)

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
