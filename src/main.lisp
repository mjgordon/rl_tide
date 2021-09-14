(in-package :rl)

(defparameter *current-state* nil)
(defparameter *current-key* nil)

(gamekit:defgame rl:gk-roguelike () ()
		   (:viewport-width rl-graphics::*window-width*)
		   (:viewport-height rl-graphics::*window-height*)
		   (:viewport-title "TIDE"))

(defun main()
  (setup)
  (startup))


(defun setup()
  "Load graphics"
  (gamekit:register-resource-package :keyword "../data/")
  (gamekit:define-image :curses-black "curses_black.png")
  (gamekit:define-image :curses-red "curses_red.png")
  (gamekit:define-image :curses-green "curses_green.png")
  (gamekit:define-image :curses-yellow "curses_yellow.png")
  (gamekit:define-image :curses-blue "curses_blue.png")
  (gamekit:define-image :curses-magenta "curses_magenta.png")
  (gamekit:define-image :curses-cyan "curses_cyan.png")
  (gamekit:define-image :curses-white "curses_white.png"))

(defun startup()
  (gamekit:start 'gk-roguelike)
  (rl-graphics:setup-terminal)
  (setup-keyboard)
  (setf rl-graphics::*redraw-flag* 0)
  (setf *current-state* #'state-setup)
  (run-state))


(defmethod gamekit:draw ((app gk-roguelike))
  (gamekit:draw-rect (gamekit:vec2 0 0) rl-graphics::*window-width* rl-graphics::*window-height* :fill-paint (gamekit:vec4 0 0 0 1))
  (rl-graphics:redraw))


;; Probably a dirty dirty hack. The CL-BODGE draw loop will only clear and redraw (i.e. call gamekit:draw)
;; if this method returns false. However, it will set the viewport transform and swap the buffers no matter what.
;; Asking for a 'rerender' from the world draw function sets the flag to 0, and the flag is incremented every loop
;; The system will redraw twice for every ask, to fill both of the swap buffers.
;; I have no idea how handle-drawing is supposed to be used, but this appears to work, and the other idea i had
;; involved saving and redrawing framebuffers as textures, which would have required a lot more digging. 
(defmethod ge.app::handle-drawing(system canvas ui)
  (prog1
      (> rl-graphics::*redraw-flag* 1)
    (incf rl-graphics::*redraw-flag*)))
  


(defun run-state ()
  "States are stored as function symbols, and either immediately or eventually via TC return
 the name of the next function to be run"
  (setf *current-state* (funcall *current-state*)))
  

(defun receive-key (key)
  "Destination of keyboard bindings. Assumes the current state will react to the input"
  (setf *current-key* key)
  (run-state))

(defun get-event ()
  *current-key*)


(defun setup-keyboard()
  "Create keyboard bindings"
  (gamekit:bind-any-button (lambda (key action)
			     (when (or (equal action :PRESSED) (equal action :REPEATING))
			       (receive-key key)))))
