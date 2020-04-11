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
				   :position (cons 5 5))))
  (state-title-screen))


(defun state-title-screen ()
  (clear-screen)
  (let* ((title-text "TIDE")
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

    ;; Tick once, then keep ticking until its the players turn again
    (world-tick *world*)
    (loop until (= (mod *timer* (entity-stat-speed (world-player *world*))) 0) do
	 (world-tick *world*))
    
    (redraw-world *world* 0 0 80 39)))

(defun world-tick (world)
  (mapc (lambda (entity)
	  (when (and (entity-alive entity)
		     (= (mod *timer* (entity-stat-speed entity)) 0))
	    (entity-update entity)))
	(world-entities world))
  (incf *timer*))
