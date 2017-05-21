;; Funkcia za riazane na tifove
;; 1. pravi spisyk na poligonite za riazane
(defun make-sp-rect ( / ss i broi ptlist x ss-num ent-num num el sp-rect)
  (setq ss (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "confine"))))
  (setq i 0)
  (setq broi (sslength ss))
  (while (< i broi)
    (setq ent (entget (ssname ss i)))
    (setq ptlist nil)
    (foreach x ent (if (eq 10 (car x)) (setq ptlist (cons (cdr x) ptlist))))
    (setq ss-num (ssget "_WP" ptlist (list (cons 0 "TEXT"))))
    (setq ent-num (entget (ssname ss-num 0)))
    (setq num (cdr (assoc 1 ent-num)))
    (setq el (list num (car ptlist) (caddr ptlist)))
    (setq sp-rect (cons el sp-rect))
    (setq i (+ i 1))
  )
  (setq a sp-rect)
)

;;2. reje tifove


(defun c:cut-tiff ( / sp red map comune path)
  (setq comune (substr (getvar "DWGNAME") 1 5))
  (setq path (getvar "DWGPREFIX"))
  (setq sp (make-sp-rect))
  (setq red (car sp))
  (setq sp (cdr sp))
  (while red
    (setq map (strcat path comune "0" (car red) "00_D"))
  )
)