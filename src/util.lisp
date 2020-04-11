(in-package :rl)

;;; GRAPHICS AND COLORS

(defun get-color-id (color)
  (cond ((equal color :black) 0)
	((equal color :red) 1)
	((equal color :green) 2)
	((equal color :yellow) 3)
	((equal color :blue) 4)
	((equal color :magenta) 5)
	((equal color :cyan) 6)
	((equal color :white) 7)))

(let ((spritesheet-names (list :curses-black
			       :curses-red
			       :curses-green
			       :curses-yellow
			       :curses-blue
			       :curses-magenta
			       :curses-cyan
			       :curses-white)))
  (defun get-spritesheet-name (id)
    (nth id spritesheet-names)))

(let ((color-vecs (list (gamekit:vec4 0 0 0 1)
			(gamekit:vec4 0.5 0 0 1)
			(gamekit:vec4 0 0.5 0 1)
			(gamekit:vec4 0.5 0.5 0 1)
			(gamekit:vec4 0 0 0.5 1)
			(gamekit:vec4 0.5 0 0.5 1)
			(gamekit:vec4 0 0.5 0.5 1)
			(gamekit:vec4 0.5 0.5 0.5 1))))
  (defun get-color-vec (id)
    (nth id color-vecs)))


;;; GRID MATH

(defparameter *dx* '(0 1 1 1 0 -1 -1 -1))
(defparameter *dy* '(-1 -1 0 1 1 1 0 -1))

(defun cart-to-loc (x y)
  (+ x (* y *terminal-width*)))


(defun trace-dummy ())

(defun get-stack-depth()
  (trace trace-dummy)
  (trace-dummy)
  (untrace trace-dummy))

;;; SYMBOLS

(defun concatenate-symbols (a b)
  "Combines the string representation of a and b with a hyphen and returns a new symbol"
  (intern (concatenate 'string (symbol-name a) "-" (symbol-name b))))
