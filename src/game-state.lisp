(in-package :rl)

(defparameter *game-state* nil)

(defstruct world-state
  current-map
  player
  entities
  offset)

(defun make-world-state-new ()
  (make-world-state :current-map (make-game-map-test)
		    :player (make-player-new)
		    :entities ()
		    :offset '(0 . 0)))

(defun get-event-any ()
  (get-event))

(defun get-event-specific (keys)
  (let ((event (get-event)))
    (if (member event keys) 
	event
	nil)))


(defun state-setup ()
  (setup-cell-types)
  (setup-entity-types)
  (setf *game-state* (make-world-state-new))
  (state-title-screen))

(defun state-title-screen ()
  (clear-screen)
  (let* ((title-text "LISP ROGUE")
	 (x (- (truncate *terminal-width* 2) (truncate (length title-text) 2)))
	 (y (truncate *terminal-height* 2)))
    (set-string x y title-text))
  #'state-game-normal)

(defun state-game-error ()
  (clear-screen)
  (let* ((title-text "ERROR")
	 (x (- (truncate *terminal-width* 2) (truncate (length title-text) 2)))
	 (y (truncate *terminal-height* 2)))
    (set-string x y title-text))
  #'state-game-error)


(defun state-game-normal ()
  (clear-screen)
  ;; We redraw the world last, but want to return the result of the state-switch
  (prog1 
      (let ((event (get-event-specific (append (get-direction-keys) (get-quit-keys)))))
	(case event
	  (:0
	   #'state-game-normal)
	  ((:1 :KEYPAD-1)
	   (move-entity-delta (world-state-current-map *game-state*) (world-state-player *game-state*) -1 1)
	   #'state-game-normal)
	  ((:2 :KEYPAD-2 :DOWN)
	   (move-entity-delta (world-state-current-map *game-state*) (world-state-player *game-state*) 0 1)
	   #'state-game-normal)
	  ((:3 :KEYPAD-3)
	   (move-entity-delta (world-state-current-map *game-state*) (world-state-player *game-state*) 1 1)
	   #'state-game-normal)
	  ((:4 :KEYPAD-4 :LEFT)
	   (move-entity-delta (world-state-current-map *game-state*) (world-state-player *game-state*) -1 0)
	   #'state-game-normal)
	  ((:5 :KEYPAD-5)
	   #'state-game-normal)
	  ((:6 :KEYPAD-6 :RIGHT)
	   (move-entity-delta (world-state-current-map *game-state*) (world-state-player *game-state*) 1 0)
	   #'state-game-normal)
	  ((:7 :KEYPAD-7)
	   (move-entity-delta (world-state-current-map *game-state*) (world-state-player *game-state*) -1 -1)
	   #'state-game-normal)
	  ((:8 :KEYPAD-8 :UP)
	   (move-entity-delta (world-state-current-map *game-state*) (world-state-player *game-state*) 0 -1)
	   #'state-game-normal)
	  ((:9 :KEYPAD-9)
	   (move-entity-delta (world-state-current-map *game-state*) (world-state-player *game-state*) 1 -1)
	   #'state-game-normal)
	  (otherwise
	   #'state-game-normal)))
    
    (redraw-world-state *game-state*)))
    
  

(defun redraw-world-state (state)
  "Redraws the current map"
  (let* ((map (world-state-current-map state)))
    (iterate-map map
		 (set-char x y (map-cell-graphic cell)))
    (draw-entity (world-state-player state))
    (status-line-draw)))
    

		 
		 
(defun draw-entity (entity)
  (let ((x (car (entity-position entity)))
	(y (cdr (entity-position entity))))
    (set-char x y (entity-graphic entity))))
    

	
	  
    
  
  
     
