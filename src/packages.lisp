(defpackage :rl
  (:use :cl :asdf)
  (:export :startup :gk-roguelike))

(defpackage :rl-graphics
  (:use :cl :asdf)
  (:export :setup-terminal
	   :set-char
	   :set-string
	   :clear-screen
	   :redraw))
