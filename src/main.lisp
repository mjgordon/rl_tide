(in-package :rl)

(defparameter *current-state* nil)
(defparameter *current-key* nil)

(gamekit:defgame rl:gk-roguelike () ()
		   (:viewport-width *window-width*)
		   (:viewport-height *window-height*)
		   (:viewport-title "TIDE"))

(defun main()
  (setup)
  (startup))

(defun distribute()
  (trivial-gamekit.distribution:deliver :roguelike 'rl:gk-roguelike))

(defun setup()
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
  (setup-terminal)
  (setup-keyboard)
  (setf *redraw-flag* t)
  (setf *current-state* #'state-setup)
  (run-state))


(defmethod gamekit:draw ((app gk-roguelike))
  (when *redraw-flag*
    (gamekit:draw-rect (gamekit:vec2 0 0) *window-width* *window-height* :fill-paint (gamekit:vec4 0 0 0 1))
    (redraw)))


(defun run-state ()
  (setf *current-state* (funcall *current-state*)))
  

(defun receive-key (key)
  (setf *current-key* key)
  ;;(status-line-add key)
  (run-state))

(defun get-event ()
  *current-key*)


(defun setup-keyboard()
  (gamekit:bind-any-button (lambda (key action)
			     (when (or (equal action :PRESSED) (equal action :REPEATING))
			       (receive-key key)))))
