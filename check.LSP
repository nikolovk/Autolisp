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
  (check-txt)
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
(defun c:poly-part (/)
  (map_topoclose "PART" "PART" 0 1 1)
)

(defun c:poly-edi (/)
  (map_topoclose "EDI" "EDI" 0 1 1)
)

(defun c:poly-conf (/)
  (map_topoclose "CONF" "CONFINE" 0 1 1)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Pravi proverka i otkriva topologia


(defun c:topo-edi (/)
  (topo-build "EDI" "L-EDI,PART-EDI,CONF-EDI" "N-EDI")
)
(defun c:topo-part (/)
  (num-pol)
  (topo-build
    "PART"
    "L-PART,PART-EDI,CONF-EDI,L-CONFINE,TAPPO,C-TAPPO"
    "NUM-PART,NUM-TESTO"
  )
)
(defun c:topo-conf (/)
  (conf-pol)
  (topo-build
    "CONF"
    "CONF-EDI,L-CONFINE,C-TAPPO"
    "NUM-CONF"
  )
)


(defun topo-build (name	    lines    points   /	       ss_nod
		   ss_lnk   ss_ctr   name     desc     var_id
		   result   indx     error
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
      (prompt "\nERROR: Unable to build topology.")
      (setq indx -1)
      (repeat (ade_errqty)
	(setq indx (1+ indx))
	(prompt (strcat "\n   ADE ERROR " (itoa indx) ": "))
	(prompt (ade_errmsg indx))
      )
    )
    (prompt "\nTopology built successfully.")
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
		(cons 0 "TEXT") (cons 1 "X,x,X0*,Y,y") (cons 8 "N-PART"))
       )
  )
  (if ss
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq txt (itoa (+ i 501)))
	;(while (< (strlen txt) 4) (setq txt (strcat "0" txt)))
	(setq txt (strcat "X" txt))
	(setq ent (subst (cons 1 txt) (assoc 1 ent) ent))
	(entmod ent)
	(setq i (+ i 1))
      )
    )
  )

  ;; Pra
  (setq
    ss (ssget "X"
	      (list
		(cons 0 "TEXT") (cons 1 "X*,Y*") (cons 8 "N-PART"))
       )
  )
  (if ss
    (progn
      (setq broi (sslength ss)
	    i	 0
      )
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq ent (subst (cons 40 (* 1.2 scala)) (assoc 40 ent) ent))
	(entmod ent)
	(setq i (+ i 1))
      )
      (prompt (strcat "\nBroi X i XX: " (itoa broi)))
    )
  )
)


(defun c:make-out (/ ss ent ptlist)
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
  (c:make-x)
  (del-3);;;;; trie block 3 krastcheta pri koordinati
  ;(del-libr);;;;; trie LIBRETTO
  (setvar "CLAYER" "CONFINE")
  (command "-layer" "t" "LINEA,LINEA-OUT" "on" "LINEA,LINEA-OUT*" "")
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
)






(defun c:num-edi
       (/ ss broi x ptlist ss_pol pt pt2 ent ent-p cod ht err ent)
  (setvar "CLAYER" "N-EDI")
  (setvar "CMDECHO" 0)
  (command "_zoom" "e")
  (num-pol)
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
  (setq ss (ssget "X" (list (cons 1 txt) (cons 8 "N-PART"))))
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
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 "N-Part"))))
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
)

(defun c:del-num (/ ss)
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 "NUM*"))))
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

(defun del-libr (/ ss)
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





(defun check-txt (/ ss i broi ent pt err)
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

;;;Pravi malki nomera, neobhodimi za proverkite
(defun num-pol (/ ss ent pt txt ang ht ss1 ent1 br i la la1)
  (c:del-num)
  (setq la "NUM-PART")
  (setq
    ss (ssget "X"
	      (list (cons -4 "<OR")
		    (cons 8 "N-PART,N-CONF,N-EDI")
		    (cons -4 "<AND")
		    (cons 1 "SVI*,ALL*")
		    (cons 8 "TESTO*")
		    (cons -4 "AND>")
		    (cons -4 "OR>")
		    (cons 0 "TEXT")
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
	(setq la1 (cdr (assoc 8 ent)))
	(if (= la1 "N-CONF")
	  (setq la "NUM-CONF")
	)
	(if (or (= la1 "TESTO-OUT")
		(= la1 "TESTO"))
	  (setq la "NUM-TESTO")
	)
	(if (= la1 "N-PART")
	  (setq la "NUM-PART")
	)
	(if (= la1 "N-EDI")
	  (setq la "NUM-EDI")
	)
	(setq txt (assoc 1 ent))
	(setq ang (assoc 50 ent))
	(setq ht (cdr (assoc 40 ent)))
	(setq
	  ss1 (ssget "X"
		     (list (cons 11 pt)
			   (cons 0 "LINE")
			   (cons 8 "LINEA-C,LINEA-P,LINEA-E")
		     )
	      )
	)
	(if ss1
	  (progn
	    (setq ent1 (entget (ssname ss1 0)))
	    (setq pt (cdr (assoc 10 ent1)))
	  )
	)
	(entmake (list (cons 0 "TEXT")
		       (cons 8 la)
		       (cons 10 pt)
		       txt
		       ang
		       (cons 40 (* ht 0.01))
		 )
	)
	(setq i (+ i 1))
      )
    )
    (princ "\nLipsvat nomera - N-PART,N-EDI,N-CONF")
  )
)


;;;Pravi malki nomera, neobhodimi za confine proverka
(defun conf-pol	(/ ss ent pt txt ang ht ss1 ent1 br i la la1)
  (c:del-num)
  (setq la "NUM-CONF")
  (setq
    ss (ssget "X"
	      (list (cons -4 "<OR")
		    (cons 8 "N-CONF")
		    (cons -4 "<AND")
		    (cons 1 "SVI*,ALL*")
		    (cons 8 "TESTO*")
		    (cons -4 "AND>")
		    (cons -4 "OR>")
		    (cons 0 "TEXT")
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
	(setq txt (assoc 1 ent))
	(setq ang (assoc 50 ent))
	(setq ht (cdr (assoc 40 ent)))
	(setq
	  ss1 (ssget "X"
		     (list (cons 11 pt)
			   (cons 0 "LINE")
			   (cons 8 "LINEA-C")
		     )
	      )
	)
	(if ss1
	  (progn
	    (setq ent1 (entget (ssname ss1 0)))
	    (setq pt (cdr (assoc 10 ent1)))
	  )
	)
	(entmake (list (cons 0 "TEXT")
		       (cons 8 la)
		       (cons 10 pt)
		       txt
		       ang
		       (cons 40 (* ht 0.01))
		 )
	)
	(setq i (+ i 1))
      )
    )
    (princ "\nLipsva nomer - N-CONF")
  )
)
