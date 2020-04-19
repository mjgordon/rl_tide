(in-package :rl)

(defun get-direction-keys ()
  '(:0 :1 :2 :3 :4 :5 :6 :7 :8 :9
    :KEYPAD-1 :KEYPAD-2 :KEYPAD-3 :KEYPAD-4 :KEYPAD-5 :KEYPAD-6 :KEYPAD-7 :KEYPAD-8 :KEYPAD-9 :KEYPAD-0
    :UP :DOWN :LEFT :RIGHT))


(defun get-quit-keys ()
  '(#\q))


(defun get-event-specific (keys)
  (let ((event (get-event)))
    (if (member event keys) 
	event
	nil)))


(defun state-setup ()
  (setup-cell-types)
  (setup-entity-types)
  (setf *world* (make-world-new))
  (move-entity (world-current-map *world*) (world-player *world*) 2 3)
  (dotimes (i 10)
    (world-add-entity (make-entity :entity-type (lookup-entity-type 'orc)
				   :position (cons (random 55) (random 55)))))
  (state-title-screen))


(defun state-title-screen ()
  (rl-graphics:clear-screen)
  (let* ((title-text "TIDE")
	 (x (- (truncate rl-graphics::*terminal-width* 2) (truncate (length title-text) 2)))
	 (y (truncate rl-graphics::*terminal-height* 2)))
    (rl-graphics::set-string x y title-text))
  #'state-game-normal)


(defun state-game-error ()
  (rl-graphics:clear-screen)
  (let* ((title-text "ERROR")
	 (x (- (truncate rl-graphics::*terminal-width* 2) (truncate (length title-text) 2)))
	 (y (truncate rl-graphics::*terminal-height* 2)))
    (rl-graphics::set-string x y title-text))
  #'state-game-error)


(defun state-game-normal ()
  (rl-graphics:clear-screen)
  ;; We resolve input first and ultimate return its result
  (prog1
      ;; Resolve Input
      (let ((event (get-event)))
	(when (member event (get-direction-keys))
	  (let ((delta nil))
	    (case event
	      ((:1 :KEYPAD-1) (setf delta '(-1 1)))
	      ((:2 :KEYPAD-2 :DOWN) (setf delta '(0 1)))
	      ((:3 :KEYPAD-3) (setf delta '(1 1)))
	      ((:4 :KEYPAD-4 :LEFT) (setf delta '(-1 0)))
	      ((:6 :KEYPAD-6 :RIGHT) (setf delta '(1 0)))
	      ((:7 :KEYPAD-7) (setf delta '(-1 -1)))
	      ((:8 :KEYPAD-8 :UP) (setf delta '(0 -1)))
	      ((:9 :KEYPAD-9) (setf delta '(1 -1))))
	    (when delta
	      (let ((map (world-current-map *world*))
		    (player (world-player *world*)))
	      (move-entity-delta map player (first delta) (second delta))
	      (world-adjust-offset *world* (entity-position player) 80 39)))))
	#'state-game-normal)
    
    ;; Update world
    ;; Tick once, then keep ticking until its the players turn again
    ;; First we update the cost-map based on reacting to the player
    (map-generate-dmap (world-current-map *world*) (world-player *world*))
    (world-tick *world*)
    (loop until (entity-ready-p (world-player *world*)) do
	 (world-tick *world*))

    ;; Draw world
    (redraw-world *world* 0 0 80 39)))


