;;;; 1. topo-edi - topologia na kashtite
;;;; 2. topo-conf - topologia na granicata
;;;; 3. topo-part - topologia na parcelite
;;;; 4. add-vert - dobavia nov vertex, kogato parvia i poslednia vert. ne savpadat
;;;; 5. make-x - pravi vsichki nomera X* kato X0001, X0002 .......
;;;; 6. make-out - pravi obektite SIMBOLO,TESTO i LINEA - OUT
;;;; 7. make-linea - promenia obektite L1,L5,L6.... na LINEA-OUT sas saotvetnia Linetype
;;;; 8. check-all - proveriava nomera sas strelka, visochina na tekst i obekti s obarkan tip
;;;; 9. num-edi - postavia nomera s + za vsiaka kashta
;;;; 10. (show-txt "x") pravi krag okolo nomer x
;;;; 11. check-dup - proverka za dublirani nomera na parceli
;;;; 12. del-num - iztriva nomera NUM*
;;;; 13. del-err - iztriva circle ERROR
;;;; 14. check-vis - proveriava za visiashti linii
;;;; 15. del-short - iztriva kasi linii

;;;;;;;;;;;;;;;;;;; Proverka visiashti linii mozaika
(defun c:check-moz-topo  ( / fp ss a)
  (setvar "CMDECHO" 0)
  (c:del-err)
  (make-dwg-txt)
  (setq fp (open (strcat (getvar "DWGPREFIX") "dwg.txt") "r"))
  (setq la-name (read-line fp))
  (if map-err
    (while (/= map-err (substr la-name 7 3)) (setq la-name (read-line fp)))
  )
  (setq la-name (substr la-name 6 6))
  (setq la-name (strcat la-name "-"))
  (setq flag-topo 1)
  (while (and (/= la-name nil) (= flag-topo 1))
    (c:del-num)
    (setq a (num-pol-fast  (strcat la-name "N-PART") (strcat la-name "LINEA-P") "NUM-PART"))
    (if (/= a nil)
     (topo-build
      (strcat  la-name "PART")
      (strcat  la-name "L-PART," la-name "PART-EDI," la-name "CONF-EDI," la-name "L-CONFINE," la-name "TAPPO")
      "NUM-PART,NUM-TESTO"
     )
    )
    (princ la-name)
    (setq cr-map (substr la-name 2 3))
    (if (/= flag-topo 1) (setq map-err cr-map))
    (setq la-name (read-line fp))
    (if la-name (setq la-name (substr la-name 6 6)))
    (if la-name (setq la-name (strcat la-name "-")))
  )
  (if (= flag-topo 1)
    (alert "NIAMA GRESHKI PO TOPOLOGIITE")
  )
  (close fp)
  (c:del-num)
  (textscr)
  (command "-layer" "lo" "*ORIGINAL,*PLOSHT*" "")
  (setvar "CMDECHO" 1)
)




;;;;;;;;;;;;;;;;;;; Proverka visiashti linii mozaika
(defun c:check-moz-vis  ( / fp la-name ss)
  (c:del-err)
  (make-dwg-txt)
  (setq fp (open (strcat (getvar "DWGPREFIX") "dwg.txt") "r"))
  (setq la-name (read-line fp))
  (setq la-name (substr la-name 6 6))
  (setq la-name (strcat la-name "-"))
  (while (/= la-name nil)
    (check-v (strcat la-name "L-CONFINE," la-name "CONF-EDI," la-name "C-TAPPO"))
    (check-v (strcat la-name "L-EDI," la-name "CONF-EDI," la-name "PART-EDI"))
    (check-v (strcat la-name "L-PART," la-name "PART-EDI," la-name "CONF-EDI," la-name "L-CONFINE," la-name "TAPPO," la-name "C-TAPPO"))
    (setq la-name (read-line fp))
    (if la-name (setq la-name (substr la-name 6 6)))
    (if la-name (setq la-name (strcat la-name "-")))
  )
  (setq ss (ssget "X" (list (cons 0 "CIRCLE") (cons 8 "ERROR"))))
  (if ss
    (alert "Visiashti linii")
    (alert "Niama visiashti linii")
  )
  (close fp)
  (command "-layer" "lo" "*ORIGINAL,*PLOSHT*" "")
)

(defun c:ff ( / )
  (find-part)
)

(defun find-part (/ np ss ent co)
  (while (= np nil)
    (setq np (getstring "Vyvedi nomer na parcel:"))
  )
  (setq ss (ssget "x" (list (cons 1 np) (cons 8 "*N-PART"))))
  (if ss
    (progn
      (setq ent (entget (ssname ss 0)))
      (setq co (cdr (assoc 10 ent)))
      (command "zoom" "c" co "70")
    )
    (princ "TOZI NOMER NE SYSHTESTVUVA")
  )
)


;;Pokazva 00 posledovatelno
(defun c:xx (/ ss txt co ent x)
  (if (= sp-0 nil) (setq sp-0 sp-00))
   (while (= ss nil)
    (setq x (car sp-0))
    (setq txt (car x))
    (setq sp-0 (cdr sp-0))
    (setq ss (ssget "x" (list (cons 1 txt) (cons 8 "*N-PART"))))
  )
  (setq ent (entget (ssname ss 0)))
  (setq co (cdr (assoc 10 ent)))
  (command "zoom" "c" co "70")
  (prompt "\nNomer za iztrivane ")
  (setvar "CMDECHO" 0)
  (princ x)
  (setvar "CMDECHO" 1)
  ;(find-plosht-txt txt)
)

;;;Pechat na spisyk na ekrana
(defun print-sp	(sp / a)
  (setq a (car sp))
  (setq sp (cdr sp))
  (while a
    (princ (strcat (car a) "-" (cdr a) "\n"))
    (setq a (car sp))
    (setq sp (cdr sp))
  )
)
;;; Pravi spisyk s nomera ot selektirani tekstove
(defun make-spisyk-ss ( ss / br ent i sp )
  (setq br (sslength ss))
  (setq i 0)
  (while (< i br)
    (setq ent (entget (ssname ss i)))
    (setq sp (cons (cdr (assoc 1 ent)) sp))
    (setq i (+ i 1))
  )
  (setq br sp)
)
;;;; Tyrsi element vyv spisyk
(defun tyrsi-el	(el sp / el-sp a)
  (setq el-sp (car sp))
  (setq sp (cdr sp))
  (setq a 0)
  (if (= el el-sp)
    (setq a 1)
  )
  (while (and (/= sp nil) (/= el el-sp))
    (setq el-sp (car sp))
    (setq sp (cdr sp))
    (if	(= el el-sp)
      (setq a 1)
    )
  )
  (setq el a)
)


;; Tyrsi iztriti nomera ot dwg, koito ne sa 00
(defun c:check-del-num ( / sp-orig sp-part sp-pa el-orig el-part sp-new x y sp-00-tmp z sp-A)
  (setvar "CMDECHO" 0)
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "*ORIGINAL"))))
  (if ss
    (setq sp-orig (make-spisyk-ss ss))
    (alert "LIPSVAT ORIGINAL")
  )
  (if (= sp-00 nil) (alert "PUSNI PROVERKA AV.DAL"))
  (setq sp-A sp-00)
  (while (setq z (car (car sp-A)))
    (setq sp-00-tmp (cons z sp-00-tmp))
    (setq sp-A (cdr sp-A))
  )
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "*N-PART"))))
  (setq sp-part (make-spisyk-ss ss))
  (while sp-orig
    (setq el-orig (car sp-orig))
    (setq sp-orig (cdr sp-orig))
    (if sp-00-tmp (setq x (tyrsi-el el-orig sp-00-tmp)))
    (setq y (tyrsi-el el-orig sp-part))
    (if (and (= x 0) (= y 0)) (setq sp-new (cons el-orig sp-new)))
  )
  (if sp-new
    (progn
      (setq sp-new (vl-sort sp-new '<))
      (princ "Iztriti nomera ot dwg, koito ne sa 00\n")
      (princ sp-new)
    )
    (princ "Niama iztriti nomera")
  )
  (setvar "CMDECHO" 1)
  (textscr)
)


;; Tyrsi dobaveni nomera v dwg, koito ne sa CEN
(defun c:check-new-num ( / sp-orig sp-part sp-pa el-orig el-part sp-new x y sp-00-tmp )
  (setvar "CMDECHO" 0)
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "*ORIGINAL"))))
  (if ss
    (setq sp-orig (make-spisyk-ss ss))
    (alert "LIPSVAT ORIGINAL")
  )
  (if (= sp-cen nil)  (alert "PUSNI PROVERKA AV.DAL"))
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "*N-PART"))))
  (setq sp-part (make-spisyk-ss ss))
  (while sp-part
    (setq el-part (car sp-part))
    (setq sp-part (cdr sp-part))
    (if sp-cen (setq x (tyrsi-el el-part sp-cen)))
    (setq y (tyrsi-el el-part sp-orig))
    (if (and (= x 0) (= y 0)) (setq sp-new (cons el-part sp-new)))
  )
  (if sp-new
    (progn
      (setq sp-new (vl-sort sp-new '<))
      (princ "Dobaveni nomera v dwg, koito ne sa CEN\n")
      (princ sp-new)
    )
    (princ "Niama dobaveni nomera")
  )
  (setvar "CMDECHO" 1)
  (textscr)
)



;; Pravi spisyk sp-CEN-plosht
(defun c:check-txt-plosht
       (/ fp txt map map-txt num plosht lst x i)
  (setq sp-CEN-plosht nil)
  (while (= fp nil)
    (setq dal (getfiled "Izberi fail s greshki" "c:\\cat\\" "dal" 4))
    (setq fp (open dal "r"))
  )
  (setq txt (read-line fp))
  (while (/= (substr txt 1 3) "CEN") (setq txt (read-line fp)))
  (setq txt (read-line fp))
  (while (/= txt nil)
    (setq i 1)
    (setq x (substr txt i 1))
    (while (/= x "\,")
      (setq i (+ i 1))
      (setq x (substr txt i 1))
    )
    (setq i (+ i 1))
    (setq x (substr txt i 1))
    (if (= x "\,")
      (setq plosht "0")
      (progn
        (setq plosht "")
	(setq i (+ i 1))
	(setq x (substr txt i 1))
        (while (/= x " ")
          (setq plosht (strcat plosht x))
          (setq i (+ i 1))
          (setq x (substr txt i 1))
        )
      )
    )
    (setq num (substr txt 1 5))
    (setq num (remove-0 num))
    (setq lst (cons num plosht))
    (setq sp-CEN-plosht (cons lst sp-CEN-plosht))
    (setq txt (read-line fp))
  )
  (close fp)
  (setq	sp-CEN-plosht
	 (vl-sort
	   sp-CEN-plosht
	   (function
	     (lambda (e1 e2) (< (car e1) (car e2)))
	   )
	 )
  )
  (if sp-CEN-plosht (setq sp-CEN-plosht (clean-dup-line sp-CEN-plosht)))
)

;; Premahvane na dublirani redove ot spisyk
(defun clean-dup-line (sp / line num pl sp-new lst)
  (setq	line (car sp)
	num-old	""
  )
  (while line
    (setq num (car line))
    (setq pl (cdr line))
    (if	(/= num num-old)
      (progn
	(setq lst (cons num pl))
	(setq sp-new (cons lst sp-new))
      )
    )
    (setq sp (cdr sp))
    (setq line (car sp))
    (setq num-old num)
  )
  (setq sp-new sp-new)
)
;; Premahvane na dublirani tekstove ot spisyk
(defun clean-dup-txt (sp / txt num-old sp-new)
  (if sp (setq sp (vl-sort sp '<)))
  (setq	txt (car sp)
	num-old	""
  )
  (while txt
    (if	(/= txt num-old)
      (progn
	(setq sp-new (cons txt sp-new))
      )
    )
    (setq num-old txt)
    (setq sp (cdr sp))
    (setq txt (car sp))
  )
  (setq sp-new sp-new)
)



(defun check-dwg-plosht	(tip	    /	      spC
			 ss	       x	  spX-problem
			 spC-problem
			 ObshtBroi  broi	  obshtoCEN
			 BroiCEN  sp-part  el-part  el-p
			)
  (c:check-txt-plosht)
  (setq broi 0)
  (setq spC sp-CEN-plosht)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROVERKA ZA NOMERA CEN


  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "*N-PART"))))
  (setq sp-part (make-spisyk-ss ss))
  (setq el-part (car (car spC)))
  (setq el-p (car spC))
  (setq spC (cdr spC))
  (while el-part
    (if sp-part (setq x (tyrsi-el el-part sp-part)))
    (if (= x 0) (setq spC-problem (cons el-p spC-problem)))
    (setq el-part (car (car spC)))
    (setq el-p (car spC))
    (setq spC (cdr spC))
  )
  (if (= tip "sort")
    (setq spC-problem
	   (vl-sort
	     spC-problem
	     (function
	       (lambda (e1 e2) (< (atof (cdr e1)) (atof (cdr e2))))
	     )
	   )
    )
  )
  (princ
    "\nSpisak na vsichki nomera, koito triabva da se dobaviat v DWG-to\n"
  )
  (print-sp spC-problem)
  (setq ObshtoCen (itoa (length sp-CEN-plosht)))
  (setq BroiCen (itoa (length spC-problem)))
  (princ (strcat "\nObsht broi CEN: "
		 ObshtoCEN		 "\nOstanali CEN: "
		 broiCEN
		)
  )
  (textscr)
)




(defun c:check-txt (/ fp txt map map-txt num tip tip-map dal lst)
  (setq	sp-00 nil
	sp-cen nil
	sp-x0 nil
	sp-FAB nil
  )
  (while (= fp nil)
    (setq dal (getfiled "Izberi fail s greshki" "c:\\cat\\" "dal" 4))
    (setq fp (open dal "r"))
  )
  (if fp
    (progn
      (setq txt (read-line fp))
      (while (/= (substr txt 1 3) "CEN")
        (setq txt (substr txt 13))
	(setq num "")
	(while (/= (substr txt 1 1) ",")
	  (setq num (strcat num (substr txt 1 1)))
	  (setq txt (substr txt 2))
	)
        (setq num (remove-space num))
        (setq num (remove-0 num))
	(setq lst (cons num txt))
        (setq sp-00 (cons lst sp-00))
	(setq txt (read-line fp))
      )
      (setq txt (read-line fp))
      (while txt
        (setq num (substr txt 1 5))
        ;(setq num (remove-space num))
        (setq num (remove-0 num))
        (setq sp-cen (cons num sp-cen))
	(setq txt (read-line fp))
      )
    )
  )
  (close fp)
  ;(if sp-00 (setq sp-00 (clean-dup-txt sp-00)))
  (if sp-cen (setq sp-cen (clean-dup-txt sp-cen)))
)


(defun c:check-dwg ( / ss sp-part sp0 spC x y z sp0-problem broi spC-problem)
  (setvar "CMDECHO" 0)
  (setq broi 0)
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "*N-PART"))))
  (command "chprop" ss "" "c" "Bylayer" "")
  (c:check-txt)
  (setq sp-part (make-spisyk-ss ss))
  (setq sp0 sp-00)
  (setq spC sp-cen)
  (setq z (car sp0))
  (setq x (car z))
  (setq sp0 (cdr sp0))
  (while x
    (setq sp-p sp-part)
    (setq y (car sp-p))
    (while (and y (/= x y))
      (setq y (car sp-p))
      (setq sp-p (cdr sp-p))
      (if (= x y) (setq sp0-problem (cons x sp0-problem)))
    )
    (setq z (car sp0))
    (setq x (car z))
    (setq sp0 (cdr sp0))
  )
  (princ "\nSpisak na vsichki nomera koito triabva da se iztriat\n")
  (if sp0-problem
     (progn
        (ocveti-spisyk sp0-problem "00")
        (princ sp0-problem)
     )
     (princ sp0-problem)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROVERKA ZA NOMERA CEN
  (setq x (car spC))
  (setq spC (cdr spC))
  (while x
    (if (= (member x sp-part) nil) (setq spC-problem (cons x spC-problem)))
    (setq x (car spC))
    (setq spC (cdr spC))
  )
  (princ
    "\nSpisak na vsichki nomera, koito triabva da se dobaviat v DWG-to\n"
  )
  (princ spC-problem)
  (setvar "CMDECHO" 1)
  (setq ObshtBroi (itoa (+ (length sp-00) (length sp-cen))))
  (setq broi (itoa (+ (length sp0-problem) (length spC-problem))))
  (setq ObshtoCen (itoa (length sp-cen)))
  (setq BroiCen (itoa (length spC-problem)))
  (princ (strcat "\nObsht broi greshki: "
		 Obshtbroi		 "\nOstanali greshki: "
		 broi			 "\nObsht broi CEN: "
		 ObshtoCEN		 "\nOstanali CEN: "
		 broiCEN
		)
  )
  (textscr)
)


(defun c:del-short (/ ss broi i pt1 pt2 dist obj ent)
  (setq ss (ssget "X" (list (cons 0 "LINE"))))
  (if ss
    (progn
      (setq broi (sslength ss))
      (setq i 0)
      (while (< i broi)
	(setq dist 1)
	(setq obj (ssname ss i))
	(setq ent (entget obj))
	(setq pt1 (cdr (assoc 10 ent)))
	(setq pt2 (cdr (assoc 11 ent)))
	(setq dist (distance pt1 pt2))
	(if (< dist 0.01)
	  (command "erase" obj "")
	)
	(setq i (+ i 1))
      )
    )
  )

)


(defun c:check-all (/)
  (del-edi)
  (CheckTxtNames)
  (c:del-err)
					;(c:make-x)
  (proveri-txt)
  (check-str)
  (check-obj)
  (CheckTxtLength)
  (textscr)
)

(defun check-v (la / ss i broi ent pt1 pt2 ptlist1 ptlist2 el1 el2)
  (princ (strcat "\n" la))
  (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 la))))
  (if ss
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq pt1 (cdr (assoc 10 ent)))
	(setq pt2 (cdr (assoc 11 ent)))
	(setq ptlist1 (cons pt1 ptlist1))
	(setq ptlist1 (cons pt2 ptlist1))
	(setq i (+ i 1))
      )
      (setq i 0)
      (while ptlist1
	(setq el1 (car ptlist1))
	(setq ptlist1 (cdr ptlist1))
	(setq ptlist2 ptlist1)
	(setq test 0)
	(while ptlist2
	  (setq el2 (car ptlist2))
	  (setq ptlist2 (cdr ptlist2))
	  (if (equal el1 el2 0.0001)
	    (progn
	      (setq test 1)
	      (setq ptlist2 nil)
	      (setq ptlist1 (vl-remove el1 ptlist1))
	      (setq i (+ i 1))
	    )
	  )
	)
	(if (= test 0)
	  (show_error el1)
	)
	(if (= 0 (rem i 100))
	  (princ (strcat "\n" (itoa i)))
	)
      )
    )
  )
)

(defun c:check-vis (/ ss)
  (c:del-err)
  (check-v "L-CONFINE,CONF-EDI,C-TAPPO")
  (check-v "L-EDI,CONF-EDI,PART-EDI")
  (check-v "L-PART,PART-EDI,CONF-EDI,L-CONFINE,TAPPO,C-TAPPO")
  (setq ss (ssget "X" (list (cons 0 "CIRCLE") (cons 8 "ERROR"))))
  (if ss
    (alert "Visiashti linii")
    (alert "Niama visiashti linii")
  )
)

;;; pravi zatvoreni polilinii za parcelite
(defun poly-part (/)
  (if (map_topoclose "PART" "PART" 0 1 1)
    (princ "\nTOPOLOGIA PART  -  OK")
    (alert "LIPSVA TOPOLOGIA PART")
  )
)

(defun poly-edi (/)
  (if (map_topoclose "EDI" "EDI" 0 1 1)
    (princ "\nTOPOLOGIA EDI  -  OK")
    (alert "LIPSVA TOPOLOGIA EDI")
  )
)

(defun poly-conf (/)
  (if (map_topoclose "CONF" "CONFINE" 0 1 1)
    (princ "\nTOPOLOGIA CONF  -  OK")
    (alert "LIPSVA TOPOLOGIA CONF")
  )
)

;;IZTRIVANE NA PART, EDI, CONF i SVI
(defun del-poligon (/ ss)
  (setq ss (ssget "X" (list (cons 8 "PART"))))
  (if ss
    (command "_ERASE" ss "")
  )
  (setq ss (ssget "X" (list (cons 8 "EDI"))))
  (if ss
    (command "_ERASE" ss "")
  )
  (setq ss (ssget "X" (list (cons 8 "CONFINE"))))
  (if ss
    (command "_ERASE" ss "")
  )
  (setq ss (ssget "X" (list (cons 8 "SVI"))))
  (if ss
    (command "_ERASE" ss "")
  )
)

(defun poly-all ( / )
  (del-poligon)
  (poly-part)
  (poly-conf)
  (poly-edi)
  (textscr)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Pravi proverka i otkriva topologia


(defun c:topo-edi (/)
  (c:del-num)
  (num-pol-fast "N-EDI" "LINEA-E" "NUM-EDI")
  (topo-build "EDI" "L-EDI,PART-EDI,CONF-EDI" "N-EDI")
)
(defun c:topo-part (/)
  (c:del-num)
  (num-pol-fast "N-PART" "LINEA-P" "NUM-PART")
  (topo-build
    "PART"
    "L-PART,PART-EDI,CONF-EDI,L-CONFINE,TAPPO,C-TAPPO"
    "NUM-PART,NUM-TESTO"
  )
)
(defun c:topo-conf (/)
  (c:del-num)
  (num-pol-fast "N-CONF" "LINEA-C" "NUM-CONF")
  (topo-build
    "CONF"
    "CONF-EDI,L-CONFINE,C-TAPPO"
    "NUM-CONF,NUM-TESTO"
  )
)



(defun topo-build (name	    lines    points   /	       ss_nod
		   ss_lnk   ss_ctr   desc     var_id   result
		   indx	    error
		  )
  (setvar "CLAYER" "0")
  (c:del-short)
  (setq error *error*)
  ;;
  ;; Define error handler
  ;;
  (defun *error* (msg)
    (alert msg)
    (setq *error* error)
    (exit)
  )
  (tpm_mnterase name)
  (setq desc "1")
  (setq var_id (tpm_varalloc))
  (tpm_varset var_id "INCOMPLETE_AREA_COLOR" 1)
  (tpm_varset var_id "CROSS_COLOR" 1)
  (tpm_varset var_id "CREATE_CNTR" 0)
  (tpm_varset var_id "CREATE_NODE" 0)
  (tpm_varset var_id "CREATE_MARKERS" 1)
  (tpm_varset var_id "CREATE_VIEW" 0)
  (if (= name "EDI")
    (progn
      (tpm_varset var_id "STOP_AT_MISSING_CNTR" 0)
      (tpm_varset var_id "CREATE_CNTR" 1)
      (tpm_varset var_id "CNTR_LAYER" "N-EDI")
    )
    (tpm_varset var_id "STOP_AT_MISSING_CNTR" 1)
  )
  (setq ss_nod nil)
  (setq ss_lnk (ssget "X" (list (cons 0 "LINE") (cons 8 lines))))
  (setq ss_ctr (ssget "X" (list (cons 0 "TEXT,POINT") (cons 8 points))))
  (setq result (tpm_mntbuild var_id name desc 3 ss_nod ss_lnk ss_ctr))
  (if (null result)
    (progn
      (setq flag-topo 0)
      (prompt "\nERROR: Unable to build topology.")
      (setq indx -1)
      (repeat (ade_errqty)
	(setq indx (1+ indx))
	(prompt (strcat "\n   ADE ERROR " (itoa indx) ": "))
	(prompt (ade_errmsg indx))
      )
    )
    (progn
      (setq flag-topo 1)
      (prompt "\nTopology built successfully.")
    )
  )
  (setq *error* error)
  (prompt "\nProcessing completed.")
  (princ)
  (textscr)
)




;; Dobavia se nov vertex, taka che poliliniata da zapochva i da zavarshva v edna tochka
(defun c:add-vert (/ ss i broi ent pt pt1)
  (setq	ss (ssget "X"
		  (list	(cons 0 "LWPOLYLINE")
			(cons 8 "EDI,CONFINE,PART,SVI")
			(cons 70 1)
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
	(setq pt (assoc 10 ent))
	(setq ent (reverse ent))
	(setq pt1 (assoc 10 ent))
	(if (not (equal pt pt1))
	  (setq ent (cons pt ent))
	)
	(setq ent (reverse ent))
	(entmod ent)
	(setq i (+ i 1))
      )
					; (alert (strcat "Broi polygon: " (itoa broi)))
    )
  )
)



;;; Pravi vsichki nomera X* kato X0001, X0002 .......
(defun c:make-x	(/ ss i broi ent txt ht)
  (if (= scala nil)
    (set-scala)
  )
;;;;; Pravi X1,X2,....
  ;;  (setq
  ;;    ss (ssget "X"
  ;;	      (list
  ;;		(cons -4 "<NOT")
  ;;		(cons 1 "XX*")
  ;;		(cons -4 "NOT>")
  ;;		(cons 0 "TEXT") (cons 1 "Y,y") (cons 8 "N-PART"))
  ;;      )
  ;;  )
  ;;  (if ss
  ;;    (progn
  ;;      (setq broi (sslength ss)
  ;;	    i	 0
  ;;      )
  ;;      (while (< i broi)
  ;;	(setq ent (entget (ssname ss i)))
  ;;	(setq txt (itoa (+ i 1)))
  ;;	;(while (< (strlen txt) 4) (setq txt (strcat "0" txt)))
  ;;	(setq txt (strcat "X" txt))
  ;;	(setq ent (subst (cons 1 txt) (assoc 1 ent) ent))
  ;;	(entmod ent)
  ;;	(setq i (+ i 1))
  ;;      )
  ;;    )
  ;;  )

;;;;; Pravi X501,X502,.... za parceli bez nomer
  (setq
    ss (ssget "X"
	      (list
		(cons -4 "<NOT")
		(cons 1 "XX*")
		(cons -4 "NOT>")
		(cons 0 "TEXT")
		(cons 1 "X,x,X*,Y,y")
		(cons 8 "*N-PART")
	      )
       )
  )
  (if ss
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (alert (itoa broi))
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq txt (itoa (+ i 1)))
					;(setq txt (itoa (+ i 501)))
	(while (< (strlen txt) 3) (setq txt (strcat "0" txt)))
	(setq txt (strcat "X" txt))
	(setq ent (subst (cons 1 txt) (assoc 1 ent) ent))
	(entmod ent)
	(setq i (+ i 1))
      )
    )
  )
)


(defun c:make-out (/ ss ent ptlist)
  (c:del-err)
  (setvar "CMDECHO" 0)
  (setq
    ss (ssget "X"
	      (list (cons 0 "INSERT") (cons 8 "SIMBOLO,SIMBOLO-OUT"))
       )
  )
  (if ss
    (command "chprop" ss "" "la" "SIMBOLO-OUT" "")
  )
  (setq
    ss (ssget "X"
	      (list (cons 0 "LWPOLYLINE") (cons 8 "LINEA,LINEA-OUT"))
       )
  )
  (if ss
    (command "chprop" ss "" "la" "LINEA-OUT" "")
  )
  (setq	ss (ssget "X"
		  (list (cons 0 "TEXT") (cons 8 "TESTO,TESTO-OUT"))
	   )
  )
  (if ss
    (command "chprop" ss "" "la" "TESTO-OUT" "")
  )
  (setq ent (entget (car (entsel "Izberi CONFINE"))))
  (command "_zoom" "e")
  (foreach x ent
    (if	(eq 10 (car x))
      (setq ptlist (cons (cdr x) ptlist))
    )
  )
  (setq	ss (ssget "_CP"
		  ptlist
		  (list (cons 0 "INSERT") (cons 8 "SIMBOLO-OUT"))
	   )
  )
  (if ss
    (command "chprop" ss "" "la" "SIMBOLO" "")
  )
  (setq	ss (ssget "_WP"
		  ptlist
		  (list (cons 0 "LWPOLYLINE") (cons 8 "LINEA-OUT"))
	   )
  )
  (if ss
    (command "chprop" ss "" "la" "LINEA" "")
  )
  (setq	ss (ssget "_WP"
		  ptlist
		  (list (cons 0 "TEXT") (cons 8 "TESTO-OUT"))
	   )
  )
  (if ss
    (command "chprop" ss "" "la" "TESTO" "")
  )
					;(c:make-x)
  ;(del-3)
;;;;; trie block 3 krastcheta pri koordinati
					;(del-libr);;;;; trie LIBRETTO
  (setvar "CLAYER" "CONFINE")
  (command "-layer"	    "t"		     "LINEA,LINEA-OUT"
	   "on"		    "LINEA,LINEA-OUT*"
	   ""
	  )
  (command "-layer" "f" "*" "t" "LINEA-OUT" "")
  (setvar "CMDECHO" 1)
)

(defun make-lin	(la / ss lt)
  (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 8 la))))
  (setq lt (substr la 2))
  (if ss
    (command "chprop" ss "" "la" "LINEA-OUT" "lt" lt "")
  )
)

(defun c:make-linea (/)
  (setvar "CMDECHO" 0)
  (make-lin "L1")
  (make-lin "L2")
  (make-lin "L5")
  (make-lin "L6")
  (make-lin "L11")
  (make-lin "L12")
  (make-lin "L29")
  (make-lin "L30")
  (make-lin "L31")
  (make-lin "L32")
  (make-lin "L33")
  (make-lin "L34")
  (make-lin "L35")
  (setvar "CMDECHO" 1)
  (c:del-err)
  (check-vp-pol)
)






(defun c:num-edi
       (/ ss broi x ptlist ss_pol pt pt2 ent ent-p cod ht err ent)
  (setvar "CLAYER" "N-EDI")
  (setvar "CMDECHO" 0)
  (command "_zoom" "e")
  (c:del-num)
  (num-pol-fast "N-PART" "LINEA-P" "NUM-PART")
  (setq
    ss (ssget "X"
	      (list (cons 0 "LWPOLYLINE") (cons 8 "EDI") (cons 70 1))
       )
  )
  (if (/= ss nil)
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq ptlist nil)
	(foreach x ent
	  (if (eq 10 (car x))
	    (setq ptlist (cons (cdr x) ptlist))
	  )
	)
	(setq ss_pol (ssget "_WP"
			    ptlist
			    (list (cons 0 "POINT") (cons 8 "N-EDI"))
		     )
	)
	(if ss_pol
	  (progn
	    (setq ent-p (ssname ss_pol 0))
	    (setq
	      ss_pol (ssget "_WP"
			    ptlist
			    (list (cons 0 "TEXT") (cons 8 "NUM-PART"))
		     )
	    )
	    (if	ss_pol
	      (progn
		(setq ent (entget (ssname ss_pol 0)))
		(setq pt (cdr (assoc 10 ent)))
		(setq cod (cdr (assoc 1 ent)))
		(setq ht (cdr (assoc 40 ent)))
		(setq cod (strcat cod "+"))
		(setq ss_pol
		       (ssget "X" (list (cons 0 "LINE") (cons 10 pt)))
		)
		(if ss_pol
		  (progn
		    (setq ent (entget (ssname ss_pol 0)))
		    (setq pt2 (cdr (assoc 11 ent)))
		    (entmake (list (cons 0 "LINE")
				   (cons 8 "LINEA-E")
				   (cons 10 pt)
				   (cons 11 pt2)
			     )
		    )
		    (setq pt pt2)
		  )
		)
		(command "_text" pt (* ht 100) 0 cod)
		(command "_ERASE" ent-p "")
	      )
	    )
	  )
	)
	(setq i (+ i 1))
      )
    )
  )
  (setq
    ss (ssget "X"
	      (list (cons 0 "LWPOLYLINE") (cons 8 "PART") (cons 70 1))
       )
  )
  (if (/= ss nil)
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq ptlist nil)
	(foreach x ent
	  (if (eq 10 (car x))
	    (setq ptlist (cons (cdr x) ptlist))
	  )
	)
	(setq ss_p (ssget "_WP"
			  ptlist
			  (list (cons 0 "POINT") (cons 8 "N-EDI"))
		   )
	)
	(if ss_p
	  (progn
	    (setq
	      ss_pol (ssget "_WP"
			    ptlist
			    (list (cons 0 "TEXT") (cons 8 "NUM-PART"))
		     )
	    )
	    (if	ss_pol
	      (progn
		(if (= (sslength ss_pol) 1)
		  (progn
		    (setq ent (entget (ssname ss_pol 0)))
		    (setq cod (cdr (assoc 1 ent)))
		    (setq cod (strcat cod "+"))
		    (setq ht (cdr (assoc 40 ent)))
		    (setq br-p (sslength ss_p)
			  j    0
		    )
		    (while (< j br-p)
		      (setq ent (entget (ssname ss_p j)))
		      (setq pt (cdr (assoc 10 ent)))
		      (command "_text" pt (* ht 100) 0 cod)
		      (setq j (+ j 1))
		    )
		    (command "_erase" ss_p "")
		  )
		  (progn
		    (setq err 1)
		    (setq pt1 (car ptlist))
		    (show_error pt1)
		  )
		)
	      )
	      (progn
		(setq err 1)
		(setq pt1 (car ptlist))
		(show_error pt1)
	      )
	    )
	  )
	)
	(setq i (+ i 1))
	(if (= 0 (rem i 10))
	  (princ (strcat "\n" (itoa i)))
	)
      )
    )
  )
  (setvar "CMDECHO" 1)
  (if (= err 1)
    (alert "Problem s nomer!!!")
  )
)


(defun show-txt	(txt / ss i broi ent pt)
  (setq ss (ssget "X" (list (cons 1 txt) (cons 8 "*N-PART"))))
  (setq	broi (sslength ss)
	i    0
  )
  (if ss
    (while (< i broi)
      (setq ent (entget (ssname ss i)))
      (setq pt (cdr (assoc 10 ent)))
      (show_error
	pt
      )
      (setq i (+ i 1))
    )
  )
)


(defun c:check-dup (/ ss broi i ent lst old new lst1 txt)
  (setq ss (ssget "X" (list (cons 0 "CIRCLE") (cons 8 "ERROR"))))
  (command "_ERASE" ss "")
  (setq ss (ssget "X" (list (cons 1 ""))))
  (command "_ERASE" ss "")
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "*N-PART"))))
  (command "chprop" ss "" "c" "bylayer" "")
  
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 "*N-Part"))))
  (setq	broi (sslength ss)
	i    0
	lst  nil
  )
  (while (< i broi)
    (setq ent (entget (ssname ss i)))
    (setq lst (cons (cdr (assoc 1 ent)) lst))
    (setq i (+ i 1))
  )
  (setq lst (acad_strlsort lst))
					;(princ lst)
  (princ "\nSpisak na vsichki nomera\n")
  (setq	i    1
	lst1 nil
  )
  (setq old (car lst))
  (princ (strcat old " "))
  (while (< i broi)
    (setq new (cadr lst))
    (princ (strcat new " "))
    (setq lst (cdr lst))
    (if	(and (= new old)
	     (/= new nil)
	     (/= new "STRADA")
	     (/= new "ACQUA")
	     (/= new "X")
	     (/= new "Y")
	     (/= new "XX500")
	)
      (setq lst1 (cons new lst1))
    )
    (setq old new)
    (setq i (+ i 1))
  )
  ;(princ lst1)
  (if lst1
    (progn
      (setq lst1 (acad_strlsort lst1))
      (setq txt (car lst1))
      (show-txt txt)
      (alert "Dublirani nomera!!!")
      (princ "\n\nSpisak na dubliranite nomera\n")
      (princ lst1)
      (textscr)
    )
    (alert (strcat "Broi parceli: " (itoa broi)))
  )
    (setvar "CMDECHO" 0)
    (ocveti-spisyk lst1 "DUP")
    (setq sp-dup lst1)
    (setvar "CMDECHO" 1)
)

(defun c:del-num (/ ss)
  ;(setvar "CMDECHO" 0)
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 "NUM*"))))
  (if ss
    (command "_ERASE" ss "")
  )
  ;(setvar "CMDECHO" 1)
)

(defun c:del-svi (/ ss)
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 "NUM-TESTO"))))
  (if ss
    (command "_ERASE" ss "")
  )
)


(defun c:del-err (/ ss)
  (setq ss (ssget "X" (list (cons 0 "CIRCLE") (cons 8 "ERROR"))))
  (if ss
    (command "_ERASE" ss "")
  )
)
(defun c:del-plosht (/ ss)
  (setq ss (ssget "X" (list (cons 8 "PLOSHT-XLS"))))
  (if ss
    (command "_ERASE" ss "")
  )
)

(defun del-edi (/ ss)
  (setq ss (ssget "X" (list (cons 0 "POINT") (cons 8 "N-EDI"))))
  (if ss
    (command "_ERASE" ss "")
  )
)

(defun del-3 (/ ss)
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 "3"))))
  (if ss
    (command "_ERASE" ss "")
  )
)

(defun del-libr	(/ ss)
  (setq ss (ssget "X" (list (cons 8 "LIBRETTO"))))
  (if ss
    (command "_ERASE" ss "")
  )
)



(defun check-obj (/ ss i broi ent pt err errall)
  (if (= scala nil)
    (set-scala)
  )
  (setq errall 0)
  (setq	ss
	 (ssget
	   "X"
	   (list
	     (cons -4 "<NOT")
	     (cons 0 "LINE")
	     (cons -4 "NOT>")
	     (cons
	       8
	       "L-PART,L-EDI,L-CONF,PART-EDI,CONF-EDI,LINEA-P,LINEA-C,LINEA-E"
	     )
	   )
	 )
  )
  (if ss
    (progn
      (if (= err nil)
	(setq err 0)
      )
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq pt (cdr (assoc 10 ent)))
	(setq err (+ err 1))
	(show_error pt)
	(setq i (+ i 1))
      )
    )
  )
  (if err
    (alert (strcat "Greshka Line - " (itoa err)))
    (setq errall 1)
  )
  (setq err nil)
  (setq	ss
	 (ssget	"X"
		(list (cons -4 "<NOT")
		      (cons 0 "TEXT")
		      (cons -4 "NOT>")
		      (cons 8 "N-PART,N-EDI,N-CONF,TESTO,TESTO-OUT")
		)
	 )
  )
  (if ss
    (progn
      (if (= err nil)
	(setq err 0)
      )
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq pt (cdr (assoc 10 ent)))
	(setq err (+ err 1))
	(show_error pt)
	(setq i (+ i 1))
      )
    )
  )
  (if err
    (alert (strcat "GreshkaTEXT - " (itoa err)))
    (setq errall 1)
  )
  (setq err nil)
  (setq	ss
	 (ssget	"X"
		(list (cons -4 "<NOT")
		      (cons 0 "LWPOLYLINE")
		      (cons -4 "NOT>")
		      (cons 8 "L1*,L5,L6,L2*,L3*,LINEA,LINEA-OUT")
		)
	 )
  )
  (if ss
    (progn
      (if (= err nil)
	(setq err 0)
      )
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq pt (cdr (assoc 10 ent)))
	(setq err (+ err 1))
	(show_error pt)
	(setq i (+ i 1))
      )
    )
  )
  (if err
    (alert (strcat "Greshka LWPOLYLINE - " (itoa err)))
    (setq errall 1)
  )
  (setq err nil)
  (setq	ss
	 (ssget	"X"
		(list (cons -4 "<NOT")
		      (cons 0 "INSERT")
		      (cons -4 "NOT>")
		      (cons 8 "SIMBOLO,SIMBOLO-OUT")
		)
	 )
  )
  (if ss
    (progn
      (if (= err nil)
	(setq err 0)
      )
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq pt (cdr (assoc 10 ent)))
	(setq err (+ err 1))
	(show_error pt)
	(setq i (+ i 1))
      )
    )
  )
  (if err
    (alert (strcat "Greshka SIMBOLO - " (itoa err)))
    (setq errall 1)
  )
  (if (= errall 1)
    (prompt "\nNiama sgresheni obekti!!!")
  )
)





(defun proveri-txt (/ ss i broi ent pt err)
  (if (= scala nil)
    (set-scala)
  )
  (setq err nil)
  (setq	ss
	 (ssget	"X"
		(list (cons -4 "<NOT")
		      (cons -4 "<OR")
		      (cons 40 (* scala 1.2))
		      (cons 40 (* scala 1.4))
		      (cons 40 (* scala 1.8))
		      (cons -4 "OR>")
		      (cons -4 "NOT>")
		      (cons 0 "TEXT")
		      (cons 8 "N-PART,N-CONF")
		)
	 )
  )
  (if ss
    (progn
      (if (= err nil)
	(setq err 0)
      )
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq pt (cdr (assoc 10 ent)))
	(setq err (+ err 1))
	(show_error pt)
	(setq i (+ i 1))
      )
    )
  )
  (if err
    (alert (strcat "Greshka visochina na text - " (itoa err)))
  )
  (setq err nil)
  (setq	ss
	 (ssget	"X"
		(list (cons -4 "<NOT")
		      (cons -4 "<OR")
		      (cons 40 (* scala 1.4))
		      (cons 40 (* scala 2.5))
		      (cons 40 (* scala 4))
		      (cons 40 (* scala 9))
		      (cons -4 "OR>")
		      (cons -4 "NOT>")
		      (cons 0 "TEXT")
		      (cons 8 "TESTO,TESTO-OUT")
		)
	 )
  )
  (if ss
    (progn
      (if (= err nil)
	(setq err 0)
      )
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq pt (cdr (assoc 10 ent)))
	(setq err (+ err 1))
	(show_error pt)
	(setq i (+ i 1))
      )
    )
  )
  (if err
    (alert (strcat "Greshka visochina na text - " (itoa err)))
    (prompt "\nNiama sgresheni visochini na TXT!!!")
  )
)


(defun CheckTxtLength (/ ss i broi ent txt pt len err)
  (setq err 0)
  (setq	ss
	 (ssget	"X"
		(list (cons 0 "TEXT") (cons 8 "TESTO,TESTO-OUT"))
	 )
  )
  (if (/= ss nil)
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq txt (cdr (assoc 1 ent)))
	(setq pt (cdr (assoc 10 ent)))
	(setq len (strlen txt))
	(if (> len 40)
	  (progn
	    (setq err (+ err 1))
	    (show_error pt)
	  )
	)
	(setq i (+ i 1))
      )
    )
  )
  (if (> err 0)
    (alert (strcat "Tekst s greshna dyljina - " (itoa err)))
    (prompt "\nNiama tekstove s greshna dyljina!!!")
  )
)

(defun CheckTxtNames (/ ss i broi ent txt lst)
  (setq	ss
	 (ssget	"X"
		(list (cons 0 "TEXT") (cons 8 "TESTO,TESTO-OUT"))
	 )
  )
  (if (/= ss nil)
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq txt (cdr (assoc 1 ent)))
	(setq lst (cons txt lst))
	(setq i (+ i 1))
      )
      (setq lst (acad_strlsort lst))
      (setq i 1)
      (while (< i broi)
	(setq txt (car lst))
	(setq lst (cdr lst))
	(print txt)
	(setq i (+ i 1))
      )
      (print lst)
    )
  )

)


(defun show_error (pt /)
  (entmake (list (cons 0 "CIRCLE")
		 (cons 8 "ERROR")
		 (cons 10 pt)
		 (cons 40 50)
		 (cons 62 1)
	   )
  )
)



(defun check-str (/ ss i broi ent pt1 pt2 la err)
  (setq	ss
	 (ssget	"X"
		(list (cons 0 "LINE") (cons 8 "LINEA-C,LINEA-E,LINEA-P"))
	 )
  )
  (if (/= ss nil)
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq pt1 (cdr (assoc 11 ent)))
	(setq pt2 (cdr (assoc 10 ent)))
	(setq la (cdr (assoc 8 ent)))
	(if (= la "LINEA-C")
	  (setq la1 "N-CONF")
	)
	(if (= la "LINEA-P")
	  (setq la1 "N-PART")
	)
	(if (= la "LINEA-E")
	  (setq la1 "N-EDI")
	)
	(setq
	  ss1 (ssget "X"
		     (list (cons 0 "TEXT") (cons 8 la1) (cons 10 pt1))
	      )
	)
	(if (not ss1)
	  (progn
	    (setq
	      ss1 (ssget
		    "X"
		    (list (cons 0 "TEXT") (cons 8 la1) (cons 10 pt2))
		  )
	    )
	    (if	ss1
	      (progn
		(setq ent (subst (cons 10 pt1) (cons 10 pt2) ent))
		(setq ent (subst (cons 11 pt2) (cons 11 pt1) ent))
		(entmod ent)
	      )
	      (progn
		(show_error pt1)
		(setq err 1)
	      )
	    )
	  )
	)
	(setq i (+ i 1))
      )
    )
  )
  (if (= err 1)
    (alert "Problem s nomer!!!")
    (prompt "\nNiama sgresheni strelki!!!")
  )
)


;;;Pravi malki nomera, neobhodimi za proverkite i cxf-out
(defun num-pol-fast (la-text	   la-line	 la-new	/      ss
		     br	    i	   pt	  pt1	 pt2	sp-line
		     sp-lines	   sp-X	  ent	 txt	ang    ht
		     x	    la
		    )
					;(c:del-num)
  (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 la-line))))
  (if ss
    (progn
      (setq br (sslength ss)
	    i  0
      )
      (repeat br
	(setq ent (entget (ssname ss i)))
	(setq pt1 (cdr (assoc 10 ent)))
	(setq pt2 (cdr (assoc 11 ent)))
	(setq sp-line (cons pt1 pt2))
	(setq sp-lines (cons sp-line sp-lines))
	(setq i (+ i 1))
      )
    )
  )
  (setq	ss (ssget "X"
		  (list	(cons 0 "TEXT")
			(cons -4 "<OR")
			(cons 8 la-text)
			(cons -4 "<AND")
			(cons 1 "SVI*,ALL*")
			(cons 8 "TESTO*")
			(cons -4 "AND>")
			(cons -4 "OR>")
		  )
	   )
  )
  (if ss
    (progn
      (setq br (sslength ss)
	    i  0
      )
      (repeat br
	(setq ent (entget (ssname ss i)))
	(setq pt (cdr (assoc 10 ent)))
	(setq la (cdr (assoc 8 ent)))
	(setq txt (assoc 1 ent))
	(setq ang (assoc 50 ent))
	(setq ht (cdr (assoc 40 ent)))
	(setq sp-X sp-lines)
	(setq pt1 (car (car sp-X)))
	(setq pt2 (cdr (car sp-X)))
	(setq sp-X (cdr sp-X))
	(while pt2
	  (if (equal pt pt2 0.0001)
	    (setq pt pt1)
	  )
	  (setq pt1 (car (car sp-X)))
	  (setq pt2 (cdr (car sp-X)))
	  (setq sp-X (cdr sp-X))
	)
	(if (or (= la "TESTO") (= la "TESTO-OUT"))
	  (setq x "NUM-TESTO")
	  (setq x la-new)
	)
	(entmake (list (cons 0 "TEXT")
		       (cons 8 x)
		       (cons 10 pt)
		       txt
		       ang
		       (cons 40 (* ht 0.01))
		 )
	)
	(setq i (+ i 1))
      )
    )
  )
)
;;;Pravi kopie na originalnite nomera
(defun num-pol-orig (la-text	   la-line	 la-new	 /      ss
		     br	    i	   pt	  pt1	 pt2	sp-line
		     sp-lines	   sp-X	  ent	 txt	ang    ht
		     x	    la
		    )
					;(c:del-num)
  (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 la-line))))
  (if ss
    (progn
      (setq br (sslength ss)
	    i  0
      )
      (repeat br
	(setq ent (entget (ssname ss i)))
	(setq pt1 (cdr (assoc 10 ent)))
	(setq pt2 (cdr (assoc 11 ent)))
	(setq sp-line (cons pt1 pt2))
	(setq sp-lines (cons sp-line sp-lines))
	(setq i (+ i 1))
      )
    )
  )
  (setq	ss (ssget "X"
		  (list	(cons 0 "TEXT")
			(cons -4 "<OR")
			(cons 8 la-text)
			(cons -4 "<AND")
			(cons 1 "SVI*,ALL*")
			(cons 8 "TESTO*")
			(cons -4 "AND>")
			(cons -4 "OR>")
		  )
	   )
  )
  (if ss
    (progn
      (setq br (sslength ss)
	    i  0
      )
      (repeat br
	(setq ent (entget (ssname ss i)))
	(setq pt (cdr (assoc 10 ent)))
	(setq la (cdr (assoc 8 ent)))
	(setq txt (assoc 1 ent))
	(setq ang (assoc 50 ent))
	(setq ht (cdr (assoc 40 ent)))
	(setq sp-X sp-lines)
	(setq pt1 (car (car sp-X)))
	(setq pt2 (cdr (car sp-X)))
	(setq sp-X (cdr sp-X))
	(while pt2
	  (if (equal pt pt2)
	    (setq pt pt1)
	  )
	  (setq pt1 (car (car sp-X)))
	  (setq pt2 (cdr (car sp-X)))
	  (setq sp-X (cdr sp-X))
	)
	(if (or (= la "TESTO") (= la "TESTO-OUT"))
	  (setq x "NUM-TESTO")
	  (setq x la-new)
	)
	(entmake (list (cons 0 "TEXT")
		       (cons 8 x)
		       (cons 10 pt)
		       txt
		       ang
		       (cons 40 (* ht 0.15))
		 )
	)
	(setq i (+ i 1))
      )
    )
  )
)
(defun original (/)
  (num-pol-orig "N-PART" "L-PART" "ORIGINAL")
  (command "-layer" "lock" "ORIGINAL" "")
)

;;;; Proverka za vpisani poligoni
(defun check-vp-pol (/ ss i broi error)
  (command "_zoom" "e")
;;;; Izbor na vsichki closed LWPOLYLINE
  (setq
    ss
     (ssget "X"
	    (list (cons 0 "LWPOLYLINE") (cons 8 "PART") (cons 70 1))
     )
  )
  (if (/= ss nil)
    (progn
      (setq i 0)
      (setq broi (sslength ss))
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq ptlist nil)
;;; Spisak ot tochki za poligona!
	(foreach x ent
	  (if (eq 10 (car x))
	    (progn
	      (setq ptlist (cons (cdr x) ptlist))
	    )
	  )
	)
;;;;;;;;;;; POLYGON V PART
	(setq ss_inside	nil
	      ss_id1 nil
	      ent_id1 nil
	)
	(setq ss_inside	(ssget "_WP"
			       ptlist
			       (list (cons 0 "LWPOLYLINE")
				     (cons 8 "PART")
				     (cons 70 1)
			       )
			)
	)
	(if (/= ss_inside nil)
	  (progn
	    (setq br_inside (sslength ss_inside))
	    (setq j 0)
	    (while (< j br_inside)
	      (setq ptlist1 nil)
	      (setq ent_ins (entget (ssname ss_inside j)))
	      (foreach x ent_ins
		(if (eq 10 (car x))
		  (progn
		    (setq ptlist1 (cons (cdr x) ptlist1))
		  )
		)
	      )
	      (setq ss_inside2 nil)
	      (setq ss_inside2
		     (ssget "_CP"
			    ptlist1
			    (list (cons 0 "LWPOLYLINE")
				  (cons 8 "PART")
				  (cons 70 1)
			    )
		     )
	      )
	      (setq br_ins2 (sslength ss_inside2))
	      (if (> br_ins2 1)
		(progn
		  (setq error 1)
		  (show_error (car ptlist1))
		  (setq j br_inside)
		  (setvar "CLAYER" "PART")
		)
	      )
	      (setq j (+ j 1))
	    )
	  )
	)
	(setq i (+ i 1))
      )
    )
  )
  (if error
    (alert "Vnimanie vpisani poligoni!!!")
  )
)




(defun c:show-dup-edi (/ ss broi i ent lst old new lst1 txt)
  (setq ss (ssget "X" (list (cons 0 "CIRCLE") (cons 8 "ERROR"))))
  (if ss
    (command "_ERASE" ss "")
  )
  (setq ss (ssget "X" (list (cons 1 ""))))
  (if ss
    (command "_ERASE" ss "")
  )
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 "N-EDI"))))
  (setq	broi (sslength ss)
	i    0
	lst  nil
  )
  (while (< i broi)
    (setq ent (entget (ssname ss i)))
    (setq lst (cons (cdr (assoc 1 ent)) lst))
    (setq i (+ i 1))
  )
  (setq lst (acad_strlsort lst))
  (setq	i    1
	lst1 nil
  )
  (setq old (car lst))
  (while (< i broi)
    (setq new (cadr lst))
    (setq lst (cdr lst))
    (if	(= new old)
      (setq lst1 (cons new lst1))
    )
    (setq old new)
    (setq i (+ i 1))
  )
  (if lst1
    (progn
      (setq lst1 (acad_strlsort lst1))
      (setq txt (car lst1))
      (setq lst1 (cdr lst1))
      (while txt
	(draw-circle txt)
	(setq txt (car lst1))
	(setq lst1 (cdr lst1))
      )
    )
  )
)

(defun draw-circle (txt / ss ent pt)
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 1 txt))))
  (setq ent (entget (ssname ss 0)))
  (setq pt (cdr (assoc 10 ent)))
  (show_error pt)
)

(defun ocveti-spisyk ( sp tip / cviat txt ss )
  (setq cviat "BYLAYER" txt "")
  (if (= tip "DUP") (setq cviat "80"))
  (if (= tip "00") (setq cviat "88"))
  (while sp
    (setq txt (strcat (car sp) "," txt))
    (setq sp (cdr sp))
  )
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 1 txt) (cons 8 "*N-PART"))))
  (command "chprop" ss "" "c" cviat "")
  (setq ss (ssget "x" (list (cons 0 "LINE") (cons 8 "*L-confine"))))
  (command "chprop" ss "" "lw" 0.4 "")
  (setvar "LWDISPLAY" 0)
)


(defun c:check-dwg-spisyk (/	       sp0	  spC	     spX
		    ss	       x	  spX-problem
		    sp0-problem		  spC-problem
		    tip-map    ObshtBroi  broi
		   )
  (setvar "CMDECHO" 0)
  (setq ss (ssget "x" (list (cons 0 "TEXT") (cons 8 "N-PART"))))
  (command "chprop" ss "" "c" "Bylayer" "")
  (c:check-txt)
  (setq tip-map (substr (getvar "DWGNAME") 10 2))
  (setq broi 0)
  (setq sp0 sp-00)
  (setq spC sp-cen)
  (setq spX sp-x0)
  (princ "\nSpisak na vsichki nomera FAB za proverka\n")
  (princ sp-FAB)
  (ocveti-spisyk sp-FAB "FAB")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PROVERKA OSNOVNA KARTA
  (if (= tip-map "00")
    (progn
      (setq x (car sp0))
      (setq sp0 (cdr sp0))
      (while x
	(setq ss
	       (ssget "X"
		      (list (cons 0 "TEXT") (cons 8 "N-PART") (cons 1 x))
	       )
	)
	(if ss
	  (setq sp0-problem (cons x sp0-problem))
	)
	(setq x (car sp0))
	(setq sp0 (cdr sp0))
      )
      (princ
	"\nSpisak na vsichki nomera 00 koito ne sa popraveni\n"
      )
      (if sp0-problem
	(progn
          (ocveti-spisyk sp0-problem "00")
          (princ sp0-problem)
        )
	(princ sp0-problem)
      )
    )
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROVERKA ZA NOMERA CEN
  (setq x (car spC))
  (setq spC (cdr spC))
  (while x
    (setq
      ss (ssget	"X"
		(list (cons 0 "TEXT") (cons 8 "N-PART") (cons 1 x))
	 )
    )
    (if	(= ss nil)
      (setq spC-problem (cons x spC-problem))
    )
    (setq x (car spC))
    (setq spC (cdr spC))
  )
  (princ
    "\nSpisak na vsichki nomera CEN koito ne syshtestvvat v DWG\n"
  )
  (princ spC-problem)
;;;;;;;;;;;;;;;;;;;;;;;;;; PROVERKA NA ALLEGATO
  (if (/= tip-map "00")
    (progn
      (setq x (car spX))
      (setq spX (cdr spX))
      (while x
	(setq ss
	       (ssget "X"
		      (list (cons 0 "TEXT") (cons 8 "N-PART") (cons 1 x))
	       )
	)
	(if ss
	  (setq spX-problem (cons x spX-problem))
	)
	(setq x (car spX))
	(setq spX (cdr spX))
      )
      (princ
	"\nSpisak na vsichki nomera ot allegati, koito ne sa popraveni\n"
      )
      (if spX-problem
	(progn
          (ocveti-spisyk spX-problem "00")
          (princ spX-problem)
        )
	(princ spX-problem)
      )

    )
    (setvar "CMDECHO" 1)
  )
  (if (= tip-map "00")
    (setq ObshtBroi (itoa (+ (length sp-00) (length sp-cen))))
  )
  (if (/= tip-map "00")
    (setq ObshtBroi (itoa (length sp-x0)))
  )
  (if (= tip-map "00")
    (setq broi (itoa (+ (length sp0-problem) (length spC-problem))))
  )
  (if (/= tip-map "00")
    (setq broi (itoa (length spX-problem)))
  )
  (setq ObshtoCen (itoa (length sp-cen)))
  (setq BroiCen (itoa (length spC-problem)))
  (princ (strcat "\nObsht broi greshki: "
		 Obshtbroi		 "\nOstanali greshki: "
		 broi			 "\nObsht broi CEN: "
		 ObshtoCEN		 "\nOstanali CEN: "
		 broiCEN
		)
  )
  (setq	PopraveniGreshki (itoa (- (atoi Obshtbroi) (atoi broi))))
  (setq	PopraveniGreshkiCEN (itoa (- (atoi ObshtoCEN) (atoi broiCEN))))
  (setq	VsichkiGreshki	   Obshtbroi
	OstanaliGreshki	   broi
	VsichkiGreshkiCEN  ObshtoCEN
	OstanaliGreshkiCEN broiCEN
  )
  (if (/= tip-map "00") (setq VsichkiGreshkiCEN "0"))
  (textscr)
)
