(defun do-edif ( / ss)
  (setq ss (ssget "x" (list (cons 8 "*+"))))
  (command "DRAWORDER" ss "f")
)