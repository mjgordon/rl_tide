(asdf:defsystem :roguelike
  :name "roguelike"
  :depends-on(:trivial-gamekit :trivial-gamekit/distribution :asdf)
  :components ((:file "packages")
	       (:file "struct-type")
	       (:file "util")
	       (:file "graphics")
	       (:file "entity")
	       (:file "game-map")
	       (:file "game-state")
	       (:file "rogue")))
	      
	       
	       		      
