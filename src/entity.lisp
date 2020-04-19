(in-package :rl)

(defstruct entity
  entity-type
  (alive t)
  position
  (behavior 'random)
  (hp 100)
  (stat-speed 10)
  (stat-attack 10))

(defstruct-type entity
  name
  graphic
  color-fore
  default-hp)

(defstruct (player (:include entity))
  gold
  slot-weapon
  slot-head
  slot-armor
  slot-boots
  slot-ring
  (has-orb nil))
  
(defun setup-entity-types ()
  (define-entity-type 'player (make-entity-type :name "Rincewind"
						:graphic #\@
						:color-fore :white
						:default-hp 100))
  (define-entity-type 'orc (make-entity-type :name "orc"
					     :graphic #\o
					     :color-fore :white
					     :default-hp 10)))

(defun make-player-new ()
  (make-player :entity-type (lookup-entity-type 'player)
	       :position (cons 2 3)))

(defun entity-ready-p (entity)
  (= (mod (read-timer) (entity-stat-speed entity))
     0))

(defun entity-update (entity)
  (case (entity-behavior entity)
    ('random (entity-move-random entity))))

(defun entity-move-random (entity)
  (let* ((dir (random 8))
	 (dx (nth dir *dx*))
	 (dy (nth dir *dy*))
	 (map (world-current-map *world*)))
    (move-entity-delta map entity dx dy)))
	   
(defun draw-player-stats (player)
  (let ((x (- rl-graphics::*terminal-width* 15)))
    (rl-graphics:set-string x 1 (entity-name player))
    (rl-graphics:set-string x 3 (format nil "HP : ~a" (player-hp player)))
    (rl-graphics:set-string x 4 (format nil "Speed : ~a" (player-stat-speed player)))
    (rl-graphics:set-string x 6 (format nil "Weapon : ~a" (player-slot-weapon player)))
    (rl-graphics:set-string x 7 (format nil "Head : ~a" (player-slot-head player)))
    (rl-graphics:set-string x 8 (format nil "Armor : ~a" (player-slot-armor player)))
    (rl-graphics:set-string x 9 (format nil "Boots : ~a" (player-slot-boots player)))
    (rl-graphics:set-string x 10 (format nil "Ring : ~a" (player-slot-ring player)))))


(defun entity-die (entity)
  (setf (entity-alive entity) nil)
  (setf (map-cell-entity (get-cell (world-current-map *world*)
				   (car (entity-position entity))
				   (cdr (entity-position entity))))
	nil))


(defun entity-attack (entity target)
  (decf (entity-hp target) (entity-stat-attack entity))
  (cond ((equal (type-of entity) 'player)
	 (status-line-add (format nil "You hit the ~a" (entity-name target))))
	((equal (type-of target) 'player)
	 (status-line-add (format nil "The ~a hits you" (entity-name entity))))
	(t
	 (status-line-add (format nil "The ~a hits the ~a" (entity-name entity) (entity-name target)))))
  (when (<= (entity-hp target) 0)
    (entity-die target)))
