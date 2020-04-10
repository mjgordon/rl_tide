(in-package :rl)

(defun mapgen-rect (map x y w h cell-type)
  "Draw a rectangle of cell-type into map of size (w,h) at position (x,y)"
  (loop for x-pos from x to (+ x w -1) do
       (set-cell-type map x-pos y cell-type)
       (set-cell-type map x-pos (+ y h -1) cell-type))
  (loop for y-pos from (+ y 1) to (+ y h -2) do
       (set-cell-type map x y-pos cell-type)
       (set-cell-type map (+ x w -1) y-pos cell-type)))
  
