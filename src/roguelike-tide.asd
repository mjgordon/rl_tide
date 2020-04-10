(asdf:defsystem :roguelike-tide
  :name "roguelike-tide"
  :depends-on(:trivial-gamekit :trivial-gamekit/distribution)
  :components ((:file "packages")
	       (:file "struct-type")
	       (:file "util")
	       (:file "graphics")
	       (:file "status-line")
	       (:file "entity")
	       (:file "game-map")
	       (:file "generator")
	       (:file "world")
	       (:file "game-state")
	       (:file "main")))
	      
	       
	       		      
