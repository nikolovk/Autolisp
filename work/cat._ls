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



(defun read-total (map / fp txt plosht sv)
  (setq fp (open map "r"))
  (setq txt (read-line fp))
  (setq tx (substr txt 1 6))
  (while (/= tx "TOTALE")
    (setq txt (read-line fp))
    (setq tx (substr txt 1 6))
    (if (= tx "SVIL-A") (setq sv (atof (substr txt 12))))
    ;(if (= sv 0) (alert map))
  )
  (close fp)
  (setq plosht (- (atof (substr txt 12)) sv))
  (setq plosht (rtos plosht))
)

(defun c:read-total-all ( / fp map map1 pl pl-obshto fp1)
  (setq pl-obshto 0)
  (setq fp1 (open "c://CAT//gotovi//plosht.txt" "a"))
  (setq fp (open "c://CAT//gotovi//sup.txt" "r"))
  (setq map (read-line fp))
  (setq map1 (strcat "c://CAT//gotovi//" map))
  (setq pl (read-total map1))
  (setq pl (remove-space pl))
  (while map
    (write-line (strcat map " - " pl) fp1)
    (setq pl (atof pl))
    (setq pl-obshto (+ pl pl-obshto))
    (setq map (read-line fp))
    (if map (setq map1 (strcat "c://CAT//gotovi//" map)))
    (if map (setq pl (read-total map1)))
    (if map (setq pl (remove-space pl)))
  )
  (close fp)
  (write-line (strcat "OBSHTO - " (rtos pl-obshto)) fp1)
  (princ (strcat "OBSHTO - " (rtos pl-obshto)))
  (close fp1)
)

(defun c:cm ( / )
  (c:change-map)
)

(defun c:change-map ( / ss ss-new ent la-new la i)
  (while (= ss nil)
    (princ "Izberi obekti:")
    (setq ss (ssget))
  )
  (while (= ss-new nil)
    (princ "Izberi obekt za nov sloi:")
    (setq ss-new (ssget))
  )
  (setq ent (entget (ssname ss-new 0)))
  (setq la-new (cdr (assoc 8 ent)))
  (setq la-new (substr la-new 2 5))
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (entget (ssname ss i)))
    (setq la (cdr (assoc 8 ent)))
    (if (and (>= (substr la 1 1) "0") (<= (substr la 1 1) "9"))
      (setq la (strcat "0" la-new (substr la 7)))
      (setq la (strcat "0" la-new "-" la))
    )
    (setq ent (subst (cons 8 la) (assoc 8 ent) ent))
    (entmod ent)
    (setq i (+ i 1))
  )
)


(defun c:moz-map ( / )
  (setq cr-map (set-map-obj))
  (c:unlo-all)
  (c:co-byl)
  (c:co-all)
  (c:co-confine)
  ;(c:lo-all)
  (c:set-back)
  (set-la)
  (command "-layer" "off" "*original,*plosht*" "")
)

(defun c:ins-la ( / )
  (command "-insert" "*C:\\cat\\source\\cat.dwg" "0,0" "" "")
)

(defun set-la ( / )
  (command "-layer" "s" (strcat "0" cr-map "00-L-CONFINE") "")
)

(defun set-map ( / )
  (setq cr-map (getstring "Vavedi tekushta karta:"))
)

(defun set-map-obj ( / ss ent la)
  (princ "Izberi karta:")
  (setq ss (ssget))
  (setq ent (entget (ssname ss 0)))
  (setq la (cdr (assoc 8 ent)))
  (setq la (substr la 2 3))
)

(defun c:f-all ( / )
  (if (= cr-map nil)
    (set-map)
  )
  ;(setq ss (ssget "x" (list (cons -4 "<NOT") (cons 8 (strcat "*" cr-map "*")) (cons -4 "NOT>"))))
  (command "-layer" "f" "*" "t" (strcat "0" cr-map "*") "")
)
(defun c:lo-all ( / )
  (if (= cr-map nil)
    (set-map)
  )
  (command "-layer" "lo" "*" "u" (strcat "0" cr-map "*") "")
)
(defun c:unlo-all ( / )
  (command "-layer" "u" "*" "")
)

(defun c:set-back ( / )
  (setq ss (ssget "x" (list (cons -4 "<NOT") (cons 8 (strcat "0" cr-map "*")) (cons -4 "NOT>"))))
  (command "draworder" ss "" "b")
)

(defun c:co-all ( / ss)
  (if (= cr-map nil)
    (set-map)
  )
  (setq ss (ssget "x" (list (cons -4 "<NOT") (cons 8 (strcat "0" cr-map "*")) (cons -4 "NOT>"))))
  (command "change" ss "" "p" "c" "7" "")
)

(defun c:co-confine ( / ss)
  (setq ss (ssget "x" (list (cons 8 "*confine") (cons -4 "<NOT") (cons 8 (strcat "0" cr-map "*")) (cons -4 "NOT>"))))
  (command "change" ss "" "p" "c" "220" "")
)


(defun c:co-byl ( / ss)
  (command "change" "ALL" "" "p" "c" "bylayer" "")
)



(defun read-sup (fp / txt num pl sp dv)
  (setq txt (read-line fp))
  (while (/= (substr txt 1 7) "FABBRIC") (setq txt (read-line fp)))
  (setq txt (read-line fp))
  (while (/= (substr txt 1 6) "PARTIC")
    (setq num (substr txt 1 9))
    (setq num (remove-space num))
    (setq pl (substr txt 10))
    (setq pl (remove-space pl))
    
    (setq dv (cons num pl))
    (setq sp (cons dv sp))
    (setq txt (read-line fp))
  )
  (close fp)
  (setq txt sp)
)

(defun read-sup-xls ( / fp txt num pl dv sp map i)
  (while (= fp nil)
    (setq map (getfiled "Select a CXF file" "c:\\cat\\" "txt" 4))
    (setq fp (open map "r"))
  )
  (setq txt (read-line fp))
  (while txt
    (while (/= (substr txt 1 1) ",") (setq txt (substr txt 2)))
    (setq num (substr txt 2 5))
    (setq num (remove-0 num))
    (setq txt (substr txt 8))
    (setq pl "")
    (if (= (substr txt 1 1) ",")
      (setq pl "0")
      (while (/= (substr txt 1 1) ",")
	(setq pl (strcat pl (substr txt 1 1)))
	(setq txt (substr txt 2))
      )
    )
    (if (or (= pl " ") (= pl "")) (setq pl "0"))
    (setq pl (remove-space pl))
    (setq dv (cons num pl))
    (setq sp (cons dv sp))
    (setq txt (read-line fp))
  )
  (close fp)
  (setq txt sp)
)

(defun c:write-sup-xls ( / sp ss)
  (setq sp (read-sup-xls))
  (command "-layer" "m" "PLOSHT-XLS" "c" "102" "" "")
  (write-sup sp "*N-PART" "PLOSHT-XLS")
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "*PLOSHT-XLS"))))
  (command "move" ss "" "0,0" "0,-2")

)

(defun write-sup (sp la la-new / ss br sp1 pl xy ent txt num)
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 la))))
  (setq br 0)
  (repeat (sslength ss)
    (setq sp1 sp)
    (setq ent (entget (ssname ss br)))
    (setq xy (cdr (assoc 10 ent)))
    (setq txt (cdr (assoc 1 ent)))
    (while (and sp1 (/= num txt))
      (setq pl (cdr (car sp1)))
      (setq num (car (car sp1)))
      (setq sp1 (cdr sp1))
      (if (= num txt) (entmake (list (cons 0 "TEXT") (cons 1 pl) (cons 8 la-new) (cons 10 xy) (cons 40 0.7))))
    )
    (setq br (+ br 1))
  )
  
)


(defun c:write-plosht ( / fp map ss sp sp1 ent xy txt x pl map1)
  (if (= scala nil)
    (set-scala)
  )
  (command "-layer" "m" "PLOSHT" "")
  (setq fp (open "c://CAT//SUP//sup.txt" "r"))
  (setq map1 (read-line fp))
  (setq map (strcat "c://CAT//SUP//" map1))
  (while map1
    (setq sp1 (read-sup map))
    (if sp1 (setq sp (append sp sp1)))
    (setq map1 (read-line fp))
    (if map1 (setq map (strcat "c://CAT//SUP//" map1)))
    ;(alert map)
  )
  (close fp)
 ; (alert "1")
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "*N-PART"))))
  (setq br 0)
  (repeat (sslength ss)
    (setq sp1 sp)
    (setq ent (entget (ssname ss br)))
    (setq xy (cdr (assoc 10 ent)))
    (setq txt (cdr (assoc 1 ent)))
    (while sp1
      (setq pl (cdr (car sp1)))
      (setq num (car (car sp1)))
      (setq sp1 (cdr sp1))
      (if (= num txt) (command "-text" xy "0.7" "0" pl ""))
    )
    (setq br (+ br 1))
  )
)





(defun popravka-txt-ang (/ ss i broi ent ht ang)
  (if (= scala nil)
    (set-scala)
  )
  (setq err nil)
  (setq	ss
	 (ssget	"X"
		(list (cons -4 "<NOT")
		      (cons 8 "ORIGINAL")
		      (cons -4 "NOT>")
		      (cons 0 "TEXT")
		      
		)
	 )
  )
  (if ss
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq ht (cdr (assoc 40 ent)))
	(setq ht (rtos ht 2 1))
	(setq ht (atof ht))
	(setq ang (cdr (assoc 50 ent)))
	(setq ang (angtos ang 0 0))
	(setq ang (angtof ang))
	(setq ent (subst (cons 40 ht) (assoc 40 ent) ent))
	(setq ent (subst (cons 50 ang) (assoc 50 ent) ent))
	(entmod ent)
	(setq i (+ i 1))
      )
    )
  )
)


(defun c:draw-ramka ( / pt1 pt2 pt3 pt4 ime)
  (setvar "CLAYER" "RASTER")
  (setq pt1 (getpoint "Posochi tochka gore liavo: "))
  (setq pt2 (getpoint "Posochi tochka dolu liavo: "))
  (setq pt3 (getpoint "Posochi tochka dolu diasno: "))
  (setq pt4 (getpoint "Posochi tochka gore diasno: "))
  (setq ime (substr (getvar "DWGNAME") 1 11))
  (setq ime (strcat ime ".TIF"))  
  (command "_text" pt1 "50" "" ime "")
  (command "_pline" pt1 pt2 pt3 pt4 "c")
)


(defun c:draw-xy ( / )
  (if (= scala nil) (set-scala))
  (setvar "snapunit" (list 50 50))
  (setvar "snapmode" 1)
  (command "snapbase" "0,0")
  (command "snapang" "0")
  (setq pt1 (getpoint "Posochi tochka dolu liavo: "))
  (setq pt2 (getpoint "Posochi tochka gore diasno: "))
  (setq x1 (car pt1))
  (setq x2 (car pt2))
  (setq y1 (cadr pt1))
  (setq y2 (cadr pt2))
  (if (< x1 0)
    (setq tx1 (rtos x1 2 0))
    (setq tx1 (strcat "+" (rtos x1 2 0)))
  )
  (if (< y1 0)
    (setq ty1 (rtos y1 2 0))
    (setq ty1 (strcat "+" (rtos y1 2 0)))
  )
  (if (< x2 0)
    (setq tx2 (rtos x2 2 0))
    (setq tx2 (strcat "+" (rtos x2 2 0)))
  )
  (if (< y2 0)
    (setq ty2 (rtos y2 2 0))
    (setq ty2 (strcat "+" (rtos y2 2 0)))
  )
  (setvar "CLAYER" "SIMBOLO")
  (command "_-insert" "3" pt1 scala "" 0)
  (setvar "CLAYER" "TESTO")
  (command "_text" (list (+ x1 (* 3 scala)) (- y1 (* 3 scala))) (* 2.5 scala) 0 (strcat "Y = " tx1) "")
  (command "_text" (list (- x1 (* 0.5 scala)) (+ y1 (* 3 scala))) (* 2.5 scala) 90 (strcat "X = " ty1) "")
  (setvar "CLAYER" "SIMBOLO")
  (command "_-insert" "3" (list x2 y1) scala "" 0)
  (setvar "CLAYER" "TESTO")
  (command "_text" (list (- x2 (* 25 scala)) (- y1 (* 3 scala))) (* 2.5 scala) 0 (strcat "Y = " tx2) "")
  (command "_text" (list (+ x2 (* 3 scala)) (+ y1 (* 3 scala))) (* 2.5 scala) 90 (strcat "X = " ty1) "")
  (setvar "CLAYER" "SIMBOLO")
  (command "_-insert" "3" pt2 scala "" 0)
  (setvar "CLAYER" "TESTO")
  (command "_text" (list (- x2 (* 25 scala)) (+ y2 (* 0.5 scala))) (* 2.5 scala) 0 (strcat "Y = " tx2) "")
  (command "_text" (list (+ x2 (* 3 scala)) (- y2 (* 25 scala))) (* 2.5 scala) 90 (strcat "X = " ty2) "")
  (setvar "CLAYER" "SIMBOLO")
  (command "_-insert" "3" (list x1 y2) scala "" 0)
  (setvar "CLAYER" "TESTO")
  (command "_text" (list (+ x1 (* 3 scala)) (+ y2 (* 0.5 scala))) (* 2.5 scala) 0 (strcat "Y = " tx1) "")
  (command "_text" (list (- x1 (* 0.5 scala)) (- y2 (* 25 scala))) (* 2.5 scala) 90 (strcat "X = " ty2) "")
  (setvar "snapmode" 0)
)

(defun c:pa ( / )
  (c:draw-map)
)


(defun c:draw-map (/ pt1 pt2 ang la la1 pt3 pt4 pt5 ent obj sp-pt)
  (setvar "SAVETIME" 5)
  (command "qsave")
  (setvar "CLAYER" "L-PART")
  (setq pt1 (getpoint "Nachalna tochka: "))
  (repeat 1000
    (initget
      "eXt Tins Reak Part peDi Edi Sline TAppo Conf CEdi CTappo Undo"
    )
    (setq pt2
	   (getpoint
	     pt1
	     (strcat
	       "\nSledvashta tochka ili [eXit/Tins/Reak/Part/peDi/Edi/Sline/TAppo/Conf/CEdi/CTappo/Undo]<"
	       (getvar "CLAYER") ">: "
	     )
	   )
    )
    (cond
      ((= pt2 "Sline")
       (c:change_e2)
      )
      ((or (= pt2 "eXt") (= pt2 nil)) (exit))
      ((= pt2 "Reak")
       (setq obj (entsel "\nIzberi obekt i tochka: "))
       (setq ent (entget (car obj)))
       (setq pt3 (cadr obj))
       (setq pt3 (osnap pt3 "_near"))
       (setq pt4 (cdr (assoc 10 ent)))
       (setq pt5 (cdr (assoc 11 ent)))
       (setq la (cdr (assoc 8 ent)))
       (setq la1 (getvar "CLAYER"))
       (command "_line" pt1 pt3 "")
       (setvar "CLAYER" la)
       (command "_line" pt4 pt3 "")
       (command "_line" pt3 pt5 "")
       (command "_erase" obj "")
       (setvar "CLAYER" la1)
       (setq pt1 pt3)
      )
      ((= pt2 "Tins")
       (if (= scala nil)
	 (set-scala)
       )
       (setq la (getvar "CLAYER"))
       (setvar "CLAYER" "SIMBOLO")
       (setq ang (getangle pt1 "Posochi agal za zavyrtane: "))
       (if (= ang nil)
	 (setq ang 0)
       )
       (setq ang (angtos ang 0 4))
       (command "_-insert" "2" pt1 scala "" ang)
       (setvar "CLAYER" la)
      )
      ((= pt2 "Part") (setvar "CLAYER" "L-PART"))
      ((= pt2 "peDi")
       (setvar "CLAYER" "PART-EDI")
      )
      ((= pt2 "Edi")
       (setvar "CLAYER" "L-EDI")
      )
      ((= pt2 "TAppo")
       (setvar "CLAYER" "TAPPO")
      )
      ((= pt2 "Conf")
       (setvar "CLAYER" "L-CONFINE")
      )
      ((= pt2 "CEdi")
       (setvar "CLAYER" "CONF-EDI")
      )
      ((= pt2 "CTappo")
       (setvar "CLAYER" "C-TAPPO")
      )
      ((= pt2 "Undo")
       (command "_erase" (entlast) "")
       (setq pt1 (car sp-pt))
       (setq sp-pt (cdr sp-pt))
      )

      ((= (type pt2) 'LIST)
       (command "_line" pt1 pt2 "")
       (if (not sp-pt)
	 (setq sp-pt pt1)
	 (setq sp-pt (cons pt1 sp-pt))
       )
       (setq pt1 pt2)
      )
      (T nil)
    )
  )
)







(defun c:change_E2 (/ obj a b c d x ang en la)
  (if (= scala nil) (set-scala))
  (setq la (getvar "clayer"))
  (setvar "CELTYPE" "ByLayer")
  (setq obj (entsel "\nIzberi strana za S:"))
  (setq en (entget (car obj)))
  (setq a (cdr (assoc 10 en)))
  (setq b (cdr (assoc 11 en)))
  (setq	c (/ (+ (car a) (car b)) 2)
	d (/ (+ (cadr a) (cadr b)) 2)
  )
  (setq x (list c d))
  (setvar "clayer" "SIMBOLO")
  (setq ang (angle a b))
  (setq ang (atof (angtos ang 0 3)))
  (command "-insert" "9" x scala "" (+ ang 90))
  (setvar "clayer" la)
)






(defun C:E1 ()
  (DrawHouse "L-EDI")
  (c:change_E2)
)


(defun C:NP ()
  (setvar "CELTYPE" "ByLayer")
  (N1 "N-PART" "LINEA-P")
)

(defun C:LS ()
  (setvar "CELTYPE" "ByLayer")
  (DrawLineInLayer "LINEA-P")
)
(defun C:L1 ()
  (setvar "CELTYPE" "ByLayer")
  (DrawPLineInLayer "L1")
)
(defun C:L5 ()
  (setvar "CELTYPE" "ByLayer")
  (DrawPLineInLayer "L5")
)
(defun C:L6 ()
  (setvar "CELTYPE" "ByLayer")
  (DrawPLineInLayer "L6")
)
(defun C:L11 ()
  (setvar "CELTYPE" "ByLayer")
  (DrawPLineInLayer "L11")
)
(defun C:L29 ()
  (setvar "CELTYPE" "ByLayer")
  (DrawPLineInLayer "L29")
)
(defun C:L30 ()
  (setvar "CELTYPE" "ByLayer")
  (DrawPLineInLayer "L30")
)
(defun C:L31 ()
  (setvar "CELTYPE" "ByLayer")
  (DrawPLineInLayer "L31")
)
(defun C:L32 ()
  (setvar "CELTYPE" "ByLayer")
  (DrawPLineInLayer "L32")
)
(defun C:L33 ()
  (setvar "CELTYPE" "ByLayer")
  (DrawPLineInLayer "L33")
)


(defun set-scala ( / )
  (initget 7)
  (setq scala (getreal "Vavedi scala:"))
  (setq scala (/ scala 1000))
)

(defun DrawLineInLayer (la /)
  (setvar "CELTYPE" "ByLayer")
  (if (/= nil la)
    (setvar "clayer" la)
  )
  (command "_line")
)
(defun DrawPLineInLayer (la /)
  (setvar "CELTYPE" "ByLayer")
  (if (/= nil la)
    (setvar "clayer" la)
  )
  (command "_pline")
)

(defun InsSim (name la / pt ang)
  (setvar "CELTYPE" "ByLayer")
  (if (= scala nil) (set-scala))
  (setvar "clayer" la)
  (setq pt (getpoint "Posochi tochka za vmakvane: "))
  (setq ang (getangle pt "Posochi agal za zavyrtane: "))
  (if (= ang nil) (setq ang 0))
  (setq ang (angtos ang 0 4))
  (command "_-insert" name pt scala "" ang)
)

(defun c:ins2 ( / )
  (inssim "2" "SIMBOLO")
)
(defun c:ins6 ( / )
  (inssim "6" "SIMBOLO")
)
(defun c:ins11 ( / )
  (inssim "11" "SIMBOLO")
)



(defun c:n-map ( / pt1 pt2 txt)
  (if (= scala nil) (set-scala))
  (setq pt2 (getpoint "Posochi tochka za vmakvane na NOMERA: "))
  (setq pt1 (getpoint "Posochi tochka za nachalo na STRELKATA: "))
  (setq txt (substr (getvar "DWGNAME") 1 11))
		    (entmake (list (cons 0 "LINE")
				   (cons 8 "LINEA-C")
				   (cons 10 pt1)
				   (cons 11 pt2)
			     )
		    )
  (setvar "clayer" "N-CONF")
  (command "_text" pt2 (* 1.8 scala) 0 txt "")
)

(defun c:ins-croce ( / pt1 i)
  (if (= scala nil) (set-scala))
  (setvar "clayer" "L1")
  (setq pt1 (getpoint "Posochi tochka za vmakvane na L1-CROCE: "))
    (command "_-insert" "*L1-croce" pt1 scala "" 0)

)


(defun c:p-trig ( / pt1 pt2 txt)
  (setvar "CELTYPE" "ByLayer")
  (if (= scala nil) (set-scala))
  (setvar "clayer" "SIMBOLO")
  (setq pt1 (getpoint "Posochi tochka za vmakvane na SIMBOLO: "))
  (setq pt2 (getpoint "Posochi tochka za vmakvane na TEXT: "))
  (setq txt (getstring "\nTEXT: "))
  (command "_-insert" "8" pt1 scala "" 0)
  (setvar "clayer" "TESTO")
  (command "_text" pt2 (* 1.4 scala) 0 txt "")
)

(defun c:fidu ( / pt1 pt2 txt)
  (setvar "CELTYPE" "ByLayer")
  (if (= scala nil) (set-scala))
  (setvar "clayer" "FIDUCIALE")
  (setq pt1 (getpoint "Posochi tochka za vmakvane na SIMBOLO: "))
  (setq pt2 (getpoint "Posochi tochka za vmakvane na TEXT: "))
  (setq txt (getstring "\nTEXT: "))
  (command "_-insert" "20" pt1 scala "" 0)
  (command "_line" pt1 pt2 "")
  (command "_text" pt2 (* 1.4 scala) 0 txt "")
)


(defun InsTXT (la h / pt ang txt)
  (setvar "CELTYPE" "ByLayer")
  (if (= scala nil) (set-scala))
  (setvar "clayer" la)
  (setq pt (getpoint "\nPosochi tochka za vmakvane na teksta: "))
  (setq ang (getangle pt "\nPosochi agal za zavyrtane: "))
  (if (= ang nil) (setq ang 0))
  (setq ang (angtos ang 0 4))
  (setq txt (getstring T "\nTEXT: "))
  (command "_text" pt (* h scala) ang txt "")
)
(defun InsTXTort (la h / pt ang txt)
  (setvar "CELTYPE" "ByLayer")
  (if (= scala nil) (set-scala))
  (setvar "clayer" la)
  (setq pt (getpoint "\nPosochi tochka za vmakvane: "))
  (setq txt (getstring T "\nTEXT: "))
  (command "_text" pt (* h scala) 0 txt)
)

(defun c:t-chiesa ( / )
  (if (= scala nil) (set-scala))
  (instxtort "N-PART" 1.8)
)

(defun c:num-strada ( / pt ang txt)
  (if (= scala nil) (set-scala))
  (setvar "clayer" "N-PART")
  (setq pt (getpoint "\nPosochi tochka za vmakvane na nomer STRADA: "))
  (setq txt "STRADA")
  (command "_text" pt (* 1.8 scala) 0 txt)
)

(defun c:num-acqua ( / pt ang txt)
  (if (= scala nil) (set-scala))
  (setvar "clayer" "N-PART")
  (setq pt (getpoint "\nPosochi tochka za vmakvane na nomer ACCQUA: "))
  (setq txt "ACQUA")
  (command "_text" pt (* 1.8 scala) 0 txt)
)


(defun names ( / fp lst)
  (setq fp (open "c:\\cat\\source\\names.txt" "r"))
  (if fp
    (progn
      (setq txt (read-line fp))
      (while (/= txt nil)
	(setq lst (cons txt lst))
	(setq txt (read-line fp))
      )
      (close fp)
      (acad_strlsort lst)
    )
    (prompt "Lipsva fail C:\CAT\SOURCE\names.txt")
  )
)

(defun C:t-str ( / pt ang txt lst lst1 fp)
  (if (= scala nil) (set-scala))
  (setq lst (list "Strada" "comunale" "vicinale" "provinciale" "statale"
                    "Comunale" "Vicinale" "Provinciale" "Statale" "Via"))
  (setq lst (append lst (names)))
  (setvar "clayer" "TESTO")
  (setq txt (box lst))
  (setq pt (getpoint "\nPosochi tochka za vmakvane na teksta: "))
  (setq ang (getangle pt "\nPosochi agal za zavyrtane: "))
  (if (= ang nil) (setq ang 0))
  (setq ang (angtos ang 0 4))
  (command "_text" pt (* 2.5 scala) ang txt "")
)

(defun C:t-acq ( / pt ang txt lst lst1 fp)
  (if (= scala nil) (set-scala))
  (setq lst (list "Fosso" "Torrente" "Fiume" "Rio" "Roggia" "Cavo" "Condotta" "Condotto" "Scolo"))
  (setq lst (append lst (names)))
  (setvar "clayer" "TESTO")
  (setq txt (box lst))
  (setq pt (getpoint "\nPosochi tochka za vmakvane na teksta: "))
  (setq ang (getangle pt "\nPosochi agal za zavyrtane: "))
  (if (= ang nil) (setq ang 0))
  (setq ang (angtos ang 0 4))
  (command "_text" pt (* 2.5 scala) ang txt "")
)


(defun c:t-loc ( / )
  (instxtort "TESTO" 2.5)
)

(defun c:t-out4 ( / )
  (instxt "TESTO" 4)
)

(defun C:t-out4 ( / pt ang txt lst lst1 fp)
  (if (= scala nil) (set-scala))
  (setq lst (list "Foglio" "F o g l i o" "F  o  g  l  i  o"
		    "Comune" "C o m u n e" "C  o  m  u  n  e" "COMUNE"
		    "Provincia" "P r o v i n c i a" "PROVINCIA"))
  (setq lst (append lst (names)))
  (setvar "clayer" "TESTO")
  (setq txt (box lst))
  (setq pt (getpoint "\nPosochi tochka za vmakvane na teksta: "))
  (setq ang (getangle pt "\nPosochi agal za zavyrtane: "))
  (if (= ang nil) (setq ang 0))
  (setq ang (angtos ang 0 4))
  (command "_text" pt (* 4 scala) ang txt "")
)



(defun c:t-sca ( / )
  (if (= scala nil) (set-scala))
  (setvar "clayer" "TESTO")
  (setq pt (getpoint "\nPosochi tochka za vmakvane: "))
  (setq txt (strcat "Scala di 1:" (rtos (* scala 1000) 2 0)))
  (command "_text" pt (* 4 scala) 0 txt "")

)

(defun comune-txt (nomer / fp txt)
  (setq name-provincia nil)
  (setq name-comune nil)
  (setq fp (open "c:\\cat\\source\\comuni.txt" "r"))
  (if fp
    (progn
      (setq txt "1")
      (while (and (/= txt nil) (= name-comune nil))
	(setq txt (read-line fp))
	(if (= txt nomer)
	  (progn
            (setq name-provincia (read-line fp))
            (setq name-comune (read-line fp))
	  )
	)
      )
    )
    (alert "Lipsva fail C:\CAT\SOURCE\comuni.txt")
  )
  (close fp)
  
)


(defun c:ime ( / pt1 pt2 pt3 txt1 txt2 txt3 x)
  (if (= scala nil) (set-scala))
  (setq nomer (substr (getvar "DWGNAME") 1 4))
  (comune-txt nomer)
  (if (= name-comune nil) (alert "Lipsva informacia za tova selishte v comuni.txt"))
  (setvar "CELTYPE" "ByLayer")
  (setvar "clayer" "TESTO")
  (setq pt1 (getpoint "\nPosochi tochka za vmakvane PROVINCIA DI ..... "))
  (if (= name-provincia nil) (setq name-provincia (getstring  T "\nPROVINCIA DI ")))
  (setq txt1 (strcat "PROVINCIA DI " name-provincia))
  (if (= name-comune nil) (setq name-comune (getstring T "\nComune di  ")))
  (setq txt2 (strcat "Comune di " name-comune))
  (setq txt3 (substr (getvar "DWGNAME") 6 4))
  (setq x (substr txt3 1 1))
  (while (= x "0")
    (setq txt3 (substr txt3 2))
    (setq x (substr txt3 1 1))
  )
  ;;(setq txt3 (getstring T "\nFoglio N: "))
  (setq txt3 (strcat "Foglio N: " txt3))
  (setq pt2 (subst (- (cadr pt1) (* 15 scala)) (cadr pt1) pt1))
  (setq pt3 (subst (- (cadr pt1) (* 30 scala)) (cadr pt1) pt1))
  (command "_text" "m" pt1 (* 4 scala) 0 txt1 "")
  (command "_text" "m" pt2 (* 9 scala) 0 txt2 "")
  (command "_text" "m" pt3 (* 4 scala) 0 txt3 "")
  
)
  
(defun c:n2 (/ t2point num_temp)
  (setvar "CELTYPE" "ByLayer")
  (if (= scala nil) (set-scala))
  (setvar "CLAYER" "N-PART")
  (if (= num_def nil)
    (setq num_def 1)
  )
  (if (= textheight nil)
    (setq textheight (* 1.8 scala))
  )
  (setq t2point (getpoint "\nPosochi tochka za vmakvane na teksta: "))
  (print (strcat "Vavedi nomer <" (itoa num_def) ">:"))
  (setq num_temp (getint))
  (if (/= num_temp nil)
    (setq num_def num_temp)
  )
  (command "-text" t2point textheight 0 (itoa num_def))
  (setq num_def (+ num_def 1))
)

(defun n1 (la la-s / num_temp inp x tip but ang point2 point1)
  (setvar "CELTYPE" "ByLayer")
  (setq ang 0)
  (if (= scala nil) (set-scala))
  (setvar "CLAYER" la)
  (if (= num_def nil)
    (setq num_def 1)
  )
  (if (= textheight nil)
    (setq textheight (* 1.8 scala))
  )
  (repeat 1000
    (print (strcat "Posochi tochka za vmakvane Strelka Zavyrtian eXit H="
		   (rtos textheight 2 1)
		   " <1><2><3><space><+><-> <"
		   (itoa num_def)
		   ">:"
	   )
    )
    (setq inp (grread))
    (setq tip (car inp))
    (setq but (cadr inp))
    (if	(= tip 3)
      (progn
	(setq point2 but)
	(if (= ang nil) (setq ang (getpoint but "Zavyrtane:")))
	(if (= ang nil)
	  (setq ang 0)
	)
	(command ".text" but textheight ang (itoa num_def))
	(setq ang 0)
	(setq num_def (+ num_def 1))
      )
    )
    (setq textheight (* 1.8 scala))    
    (if	(and (= tip 2) (= but 32))
      (setq num_def (getint "\nVavedi nomer:"))
    )
    (if	(and (= tip 2) (= but 43))
      (setq num_def (+ num_def 1))
    )
    (if	(and (= tip 2) (= but 45))
      (setq num_def (- num_def 1))
    )
    (if	(and (= tip 2) (= but 49))
      (setq textheight (* 1.8 scala))
    )
    (if	(and (= tip 2) (= but 50))
      (setq textheight (* 1.4 scala))
    )
    (if	(and (= tip 2) (= but 51))
      (setq textheight (* 1.2 scala))
    )
    (if	(and (= tip 2) (= but 122))
      (setq ang nil)
    )
    (if	(and (= tip 2) (= but 120))
      (exit)
    )
    (if	(and (= tip 2) (= but 115))
      (progn
	(setvar "CLAYER" la-s)
	(setq point1 (getpoint "\nTochka v parcela"))
	(command "line" point1 point2 "")
	(setvar "CLAYER" la)
      )
    )
  )
)

(DEFUN c:REAK (/)
  (SETQ A (ENTSEL "\n- Izberete obekt i tocka:"))
  (setq b (car a))
  (setq c (cadr a))
  (command "break" B c c)
)


