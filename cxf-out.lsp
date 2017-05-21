(defun c:cxf-out (/ map map1 ss txt scala1)
  (setvar "CMDECHO" 0)
  (command "_zoom" "e")
  (num-pol) ;;;; Pishe malki nomera, za da ne presichat granicite na parcelite

  ;; Tyrsi nadpis Scala di i chete scala-ta na kartata
  (setq scala nil)
  (setq	ss (ssget "X"
		  (list	(cons 0 "TEXT")
			(cons 8 "TESTO-OUT")
			(cons 1 "Scala di 1:*")
		  )
	   )
  )
  (if ss
    (progn
      (setq txt (cdr (assoc 1 (entget (ssname ss 0)))))
      (setq txt (substr txt 12 4))
      (if (= (substr txt 4 1) " ")
	(setq txt (substr txt 1 3))
      )
      (setq scala (atoi txt))
    )
  )
  (if (= scala nil)
    (progn
      (initget (+ 1 2 4))
      (setq scala (getint (strcat "\nVavedi scala :")))
    )
  )
  ;; Sazdava cxf fail s imeto na dwg-to i zapisva saotvetnite elementi
  (setq map1 (substr (getvar "DWGNAME") 1 11))
  (setq map (strcat (getvar "DWGPREFIX") map1 ".cxf"))
  (setq fp (open map "w"))
  (write-line "MAPPA" fp)
  (write-line map1 fp)
  (write-line (r12 (rtos scala 2 3)) fp)
  (close fp)
  (c:add-vert)
  (part-out "PART" "NUM-PART" "LINEA-P")
  (conf-out "CONFINE" "NUM-CONF" "LINEA-C" "SVI")
  (edi-out "EDI" "NUM-EDI" "LINEA-E")
  (text-out "TESTO")
  (text-out "TESTO-OUT")
  (sim-out "SIMBOLO")
  (sim-out "SIMBOLO-OUT")
  (fidu-out "FIDUCIALE")
  (fidu-out "FIDUCIALE-OUT")
  (lin-out "LINEA")
  (lin-out "LINEA-OUT")
  (lib-out)
  (raster-out)
  (setq fp (open map "a"))
  (write-line "EOF" fp)
  (close fp)
  (setq fp nil)
  (sup-out)
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 "NUM-*"))))

  (if ss
    (command "_ERASE" ss "")
  )
  (setvar "CMDECHO" 1)
  (textscr)
)





(defun edi-out	(la-edi    la-num     la-strelka /	     ss
		 ss_line    ss_txt     map	  broi	     i
		 j	    ent	       ptlist	  br_vert    x
		 y	    z	       br_id	  area-part  area-ins
		 enta	    ss_inside  ss_id1	  ss_id	     line_id
		 ent_id1    ent_id     ent_ins	  all_vert_ins
		 vert_ins   cod_ins    br_inside  br_vert_ins
		 zz	    ptlist1    cod_ident  dim_ident  ang_ident
		)




;;;; Izbor na vsichki closed LWPOLYLINE
  (setq
    ss
     (ssget "X"
	    (list (cons 0 "LWPOLYLINE") (cons 8 la-edi) (cons 70 1))
     )
  )
  (setq ar-edi 0)
  (setq n-edi 0)
  (setq broi 0)  
  (if (/= ss nil)
    (progn
      (setq map (substr (getvar "DWGNAME") 1 11))
      (setq map (strcat (getvar "DWGPREFIX") map ".cxf"))
      ;; spisak poligon - plosht
      (setq i 0)
      (setq broi (sslength ss))
      (setq n-edi broi)
;;;; za edin POLYGON
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq ptlist nil)
	(setq br_vert 0)
;;; Spisak ot tochki za poligona!
	(foreach x ent
	  (if (eq 10 (car x))
	    (progn
	      (setq ptlist (cons (cdr x) ptlist))
	      (setq br_vert (+ br_vert 1))
	    )
	  )
	)
	;; Vzimame ploshta na poligona	
	(setq area-part	0
	      area-ins 0
	)
	(setq enta (ssname ss i))
	(setq area-part (ar-pol enta))
;;;;;;;;;;; POLYGON V PART
	(setq ss_inside	nil
	      ss_id1 nil
	      ent_id1 nil
	)
	(setq all_vert_ins nil)
	(setq vert_ins nil)
	(setq cod_ins nil)
	(setq ss_inside	(ssget "_WP"
			       ptlist
			       (list (cons 0 "LWPOLYLINE")
				     (cons 8 la-edi)
				     (cons 70 1)
			       )
			)
	)
	(if (= ss_inside nil)
	  (setq br_inside 0)
	  (progn
	    (setq br_inside (sslength ss_inside))
	    (setq j 0)
	    (while (< j br_inside)
	      (setq br_vert_ins 0)
	      (setq ptlist1 nil)
	      (setq ent_ins (entget (ssname ss_inside j)))
	      (foreach x ent_ins
		(if (eq 10 (car x))
		  (progn
		    (setq br_vert (+ br_vert 1))
		    (setq br_vert_ins (+ br_vert_ins 1))
		    (setq vert_ins (cons (cadr x) vert_ins))
		    (setq vert_ins (cons (caddr x) vert_ins))
		    (setq ptlist1 (cons (cdr x) ptlist1))
		  )
		)
	      )
	      (setq all_vert_ins (cons br_vert_ins all_vert_ins))
;;;; Vzima nomera na parcela koito se namira vatre v parcel
	      (setq
		ss_id1 (ssget "_WP"
			      ptlist1
			      (list (cons 0 "TEXT") (cons 8 la-num))
		       )
	      )
	      (if (/= ss_id1 nil)
		(progn
		  (setq ent_id1 (entget (ssname ss_id1 0)))
		  (setq
		    cod_ins (cons (cdr (assoc 1 ent_id1)) cod_ins)
		  )
		)
	      )
;;; Sumira ploshta na parcelite vatre v parcel
	      (setq ent_ins (ssname ss_inside j))
	      (setq area-ins (+ area-ins (ar-pol ent_ins)))
	      (setq j (+ j 1))
	    )
	  )
	)
;;;;;;;;;;;;;; Izbor na NOMER NA PARCEL
	(setq ss_id (ssget "_WP"
			   ptlist
			   (list (cons 0 "TEXT") (cons 8 la-num))
		    )
	)
	(if (= ss_id nil)
	  (progn
	    (princ (strcat "\nPolygon bez nomer - " la-edi))
	    (setq pt (car ptlist))
	    (show_error pt)
	  )
	  (progn
	    (if	(= br_inside 0)
	      (progn
		(setq ent_id (entget (ssname ss_id 0)))
		(setq cod_ident (cdr (assoc 1 ent_id)))
	      )
	      (progn
		(setq br_id (sslength ss_id))
		(setq z 0)
		(while (< z br_id)
		  (setq ent_id (entget (ssname ss_id z)))
		  (setq cod_ident (cdr (assoc 1 ent_id)))
		  (setq y 0)
		  (foreach x cod_ins
		    (if	(= x cod_ident)
		      (setq y 1)
		    )
		  )
		  (setq z (+ z 1))
		  (if (= y 0)
		    (setq z br_id)
		  )
		)
	      )
	    )
;;;;; plosht kashti
	    (setq area-part (- area-part area-ins))
	    (setq ar-edi (+ ar-edi area-part))
	    (setq dim_ident (cdr (assoc 40 ent_id)))
	    (setq dim_ident (* dim_ident 100 10000))
	    (setq dim_ident (rtos (/ dim_ident scala) 2 0))
	    (setq dim_ident (r4 dim_ident))
	    (setq ang_ident (rtos (cdr (assoc 50 ent_id)) 2 3))
	    (setq ang_ident (r12 ang_ident))
	    (setq fp (open map "a"))
	    (write-line "BORDO" fp)
	    (write-line cod_ident fp)
	    (write-line dim_ident fp)
	    (write-line ang_ident fp)
	    (setq ss_id	nil
		  ss_line nil
	    )
;;;;; proverka za strelka
	    (setq ss_line
		   (ssget "F"
			  ptlist
			  (list (cons 0 "LINE") (cons 8 la-strelka))
		   )
	    )
	    (if	(/= ss_line nil)
	      (progn
		(setq line_id (entget (ssname ss_line 0)))
		(setq pt (cdr (assoc 10 line_id)))
		(setq ss_txt nil)
		(setq ss_txt (ssget "X"
				    (list (cons 0 "TEXT")
					  (cons 10 pt)
					  (cons 8 la-num)
				    )
			     )
		)
		(setq zz (cdr (assoc 1 (entget (ssname ss_txt 0)))))
		(if (= zz cod_ident)
		  (vertex_l line_id)
		  (progn
		    (vertex ent_id)
		    (vertex ent_id)
		  )
		)
	      )
	      (progn
		(vertex ent_id)
		(vertex ent_id)
	      )
	    )
	    (setq all_vert_ins (reverse all_vert_ins))
	    (setq vert_ins (reverse vert_ins))
	    (write-line (r4 (itoa br_inside)) fp)
	    (write-line (r4 (itoa br_vert)) fp)
	    (if	(/= br_inside 0)
	      (progn
		(foreach x all_vert_ins (write-line (r4 (itoa x)) fp))
	      )
	    )
	    (vertex ent)
	    (if	(/= br_inside 0)
	      (foreach x vert_ins
		(write-line (r12 (rtos x 2 3)) fp)
	      )
	    )
	  )
	)
	(setq i (+ i 1))
      )
      (close fp)
    )
  )
  (princ (strcat "\n" la-edi " - " (itoa broi)))
)




(defun conf-out (la-conf	   la-num     la-strelka la-svi  /	    ss
		ss_line	   ss_txt     map	 broi	    i
		j	   ent	      ptlist	 br_vert    x
		y	   z	      br_id	
		enta	   ss_inside  ss_id1	 ss_id	    line_id
		ent_id1	   ent_id     ent_ins	 all_vert_ins
		vert_ins   cod_ins    br_inside	 br_vert_ins
		zz	   ptlist1    cod_ident	 dim_ident  ang_ident
	       )




;;;; Izbor na vsichki closed LWPOLYLINE
  (setq
    ss
     (ssget "X"
	    (list (cons 0 "LWPOLYLINE") (cons 8 la-conf) (cons 70 1))
     )
  )
  (if (/= ss nil)
    (progn
      (setq map (substr (getvar "DWGNAME") 1 11))
      (setq map (strcat (getvar "DWGPREFIX") map ".cxf"))
      (setq i 0)
      (setq broi (sslength ss))
;;;; za edin POLYGON
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq ptlist nil)
	(setq br_vert 0)
;;; Spisak ot tochki za poligona!
	(foreach x ent
	  (if (eq 10 (car x))
	    (progn
	      (setq ptlist (cons (cdr x) ptlist))
	      (setq br_vert (+ br_vert 1))
	    )
	  )
	)


;;;;;;;;;;;;;; Izbor na NOMER NA PARCEL
	(setq ss_id (ssget "_WP"
			   ptlist
			   (list (cons 0 "TEXT") (cons 8 la-num))
		    )
	)
	(if (/= ss_id nil)
	  (progn
	    ;; Vzimame ploshta na poligona	
	    (setq ar-conf 0
		  ar-svil 0
		  n-svil 0
	    )
	    (setq enta (ssname ss i))
	    (setq ar-conf (ar-pol enta))


;;;;;;;;;;; POLYGON V PART
	    (setq ss_inside nil
		  ss_id1 nil
		  ent_id1 nil
	    )
	    (setq all_vert_ins nil)
	    (setq vert_ins nil)
	    (setq cod_ins nil)
	    (setq ss_inside (ssget "_CP"
				   ptlist
				   (list (cons 0 "LWPOLYLINE")
					 (cons 8 la-svi)
					 (cons 70 1)
				   )
			    )
	    )
	    (if	(= ss_inside nil)
	      (setq br_inside 0)
	      (progn
		(setq br_inside (sslength ss_inside))
		(setq n-svil br_inside)
		(setq j 0)
		(while (< j br_inside)
		  (setq br_vert_ins 0)
		  (setq ptlist1 nil)
		  (setq ent_ins (entget (ssname ss_inside j)))
		  (foreach x ent_ins
		    (if	(eq 10 (car x))
		      (progn
			(setq br_vert (+ br_vert 1))
			(setq br_vert_ins (+ br_vert_ins 1))
			(setq vert_ins (cons (cadr x) vert_ins))
			(setq vert_ins (cons (caddr x) vert_ins))
			(setq ptlist1 (cons (cdr x) ptlist1))
		      )
		    )
		  )
		  (setq all_vert_ins (cons br_vert_ins all_vert_ins))
;;; Sumira ploshta na parcelite vatre v parcel
		  (setq ent_ins (ssname ss_inside j))
		  (setq ar-svil (+ ar-svil (ar-pol ent_ins)))
		  (setq j (+ j 1))
		)
	      )
	    )



	    (setq ent_id (entget (ssname ss_id 0)))
	    (setq cod_ident (cdr (assoc 1 ent_id)))
	    (setq dim_ident (cdr (assoc 40 ent_id)))
	    (setq dim_ident (* dim_ident 100 10000))
	    (setq dim_ident (rtos (/ dim_ident scala) 2 0))
	    (setq dim_ident (r4 dim_ident))
	    (setq ang_ident (rtos (cdr (assoc 50 ent_id)) 2 3))
	    (setq ang_ident (r12 ang_ident))
	    (setq fp (open map "a"))
	    (write-line "BORDO" fp)
	    (write-line cod_ident fp)
	    (write-line dim_ident fp)
	    (write-line ang_ident fp)
	    (setq ss_id	nil
		  ss_line nil
	    )
;;;;; proverka za strelka
	    (setq ss_line
		   (ssget "F"
			  ptlist
			  (list (cons 0 "LINE") (cons 8 la-strelka))
		   )
	    )
	    (if	(/= ss_line nil)
	      (progn
		(setq line_id (entget (ssname ss_line 0)))
		(setq pt (cdr (assoc 10 line_id)))
		(setq ss_txt nil)
		(setq ss_txt (ssget "X"
				    (list (cons 0 "TEXT")
					  (cons 10 pt)
					  (cons 8 la-num)
				    )
			     )
		)
		(setq zz (cdr (assoc 1 (entget (ssname ss_txt 0)))))
		(if (= zz cod_ident)
		  (vertex_l line_id)
		  (progn
		    (vertex ent_id)
		    (vertex ent_id)
		  )
		)
	      )
	      (progn
		(vertex ent_id)
		(vertex ent_id)
	      )
	    )
	    (setq all_vert_ins (reverse all_vert_ins))
	    (setq vert_ins (reverse vert_ins))
	    (write-line (r4 (itoa br_inside)) fp)
	    (write-line (r4 (itoa br_vert)) fp)
	    (if	(/= br_inside 0)
	      (progn
		(foreach x all_vert_ins (write-line (r4 (itoa x)) fp))
	      )
	    )
	    (vertex ent)
	    (if	(/= br_inside 0)
	      (foreach x vert_ins
		(write-line (r12 (rtos x 2 3)) fp)
	      )
	    )
	  )
	)
	(setq i (+ i 1))
      )
      (close fp)
    )
  )
  (princ (strcat "\n" la-conf " - " (itoa broi)))
)






(defun part-out	(la-part    la-num     la-strelka /	     ss
		 ss_line    ss_txt     map	  broi	     i
		 j	    ent	       ptlist	  br_vert    x
		 y	    z	       br_id	  area-part  area-ins
		 enta	    ss_inside  ss_id1	  ss_id	     line_id
		 ent_id1    ent_id     ent_ins	  all_vert_ins
		 vert_ins   cod_ins    br_inside  br_vert_ins obj
		 zz	    ptlist1    cod_ident  dim_ident  ang_ident
		)




;;;; Izbor na vsichki closed LWPOLYLINE
  (setq
    ss
     (ssget "X"
	    (list (cons 0 "LWPOLYLINE") (cons 8 la-part) (cons 70 1))
     )
  )
  (if (/= ss nil)
    (progn
      (setq map (substr (getvar "DWGNAME") 1 11))
      (setq map (strcat (getvar "DWGPREFIX") map ".cxf"))
      (setq sp-pol nil)
      ;; spisak poligon - plosht
      (setq broi 0)
      (setq i 0)
      (setq broi (sslength ss))
;;;; za edin POLYGON
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq ptlist nil)
	(setq br_vert 0)
;;; Spisak ot tochki za poligona!
	(foreach x ent
	  (if (eq 10 (car x))
	    (progn
	      (setq ptlist (cons (cdr x) ptlist))
	      (setq br_vert (+ br_vert 1))
	    )
	  )
	)
	;; Vzimame ploshta na poligona	
	(setq area-part	0
	      area-ins 0
	)
	(setq enta (ssname ss i))
	(setq area-part (ar-pol enta))
;;;;;;;;;;; POLYGON V PART
	(setq ss_inside	nil
	      ss_id1 nil
	      ent_id1 nil
	)
	(setq all_vert_ins nil)
	(setq vert_ins nil)
	(setq cod_ins nil)
;	(setq obj (assoc 330 ent))
	(setq ss_inside	(ssget "_WP"
			       ptlist
			       (list
				; (cons -4 "<NOT")
				; obj
				; (cons -4 "NOT>")
				     (cons 0 "LWPOLYLINE")
				     (cons 8 la-part)
				     (cons 70 1)
			       )
			)
	)
	(if (= ss_inside nil)
	  (setq br_inside 0)
	  (progn
	    (setq br_inside (sslength ss_inside))
	    (setq j 0)
	    (while (< j br_inside)
	      (setq br_vert_ins 0)
	      (setq ptlist1 nil)
	      (setq ent_ins (entget (ssname ss_inside j)))
	      (foreach x ent_ins
		(if (eq 10 (car x))
		  (progn
		    (setq br_vert (+ br_vert 1))
		    (setq br_vert_ins (+ br_vert_ins 1))
		    (setq vert_ins (cons (cadr x) vert_ins))
		    (setq vert_ins (cons (caddr x) vert_ins))
		    (setq ptlist1 (cons (cdr x) ptlist1))
		  )
		)
	      )
	      (setq all_vert_ins (cons br_vert_ins all_vert_ins))
;;;; Vzima nomera na parcela koito se namira vatre v parcel
	      (setq
		ss_id1 (ssget "_WP"
			      ptlist1
			      (list (cons 0 "TEXT") (cons 8 la-num))
		       )
	      )
	      (if (/= ss_id1 nil)
		(progn
		  (setq ent_id1 (entget (ssname ss_id1 0)))
		  (setq
		    cod_ins (cons (cdr (assoc 1 ent_id1)) cod_ins)
		  )
		)
	      )
;;; Sumira ploshta na parcelite vatre v parcel
	      (setq ent_ins (ssname ss_inside j))
	      (setq area-ins (+ area-ins (ar-pol ent_ins)))
	      (setq j (+ j 1))
	    )
	  )
	)
;;;;;;;;;;;;;; Izbor na NOMER NA PARCEL
	(setq ss_id (ssget "_WP"
			   ptlist
			   (list (cons 0 "TEXT") (cons 8 la-num))
		    )
	)
	(if (= ss_id nil)
	  (progn
	    (princ (strcat "\nPolygon bez nomer - " la-part))
	    (setq pt (car ptlist))
	    (show_error pt)
	  )
	  (progn
	    (if	(= br_inside 0)
	      (progn
		(setq ent_id (entget (ssname ss_id 0)))
		(setq cod_ident (cdr (assoc 1 ent_id)))
	      )
	      (progn
		(setq br_id (sslength ss_id))
		(setq z 0)
		(while (< z br_id)
		  (setq ent_id (entget (ssname ss_id z)))
		  (setq cod_ident (cdr (assoc 1 ent_id)))
		  (setq y 0)
		  (foreach x cod_ins
		    (if	(= x cod_ident)
		      (setq y 1)
		    )
		  )
		  (setq z (+ z 1))
		  (if (= y 0)
		    (setq z br_id)
		  )
		)
	      )
	    )
;;;;; pravi spisak prcel - plosht - .sup
	    (setq area-part (- area-part area-ins))
	    (setq sp-pol (cons (cons cod_ident area-part) sp-pol))
	    (setq dim_ident (cdr (assoc 40 ent_id)))
	    (setq dim_ident (* dim_ident 100 10000))
	    (setq dim_ident (rtos (/ dim_ident scala) 2 0))
	    (setq dim_ident (r4 dim_ident))
	    (setq ang_ident (rtos (cdr (assoc 50 ent_id)) 2 3))
	    (setq ang_ident (r12 ang_ident))
	    (setq fp (open map "a"))
	    (write-line "BORDO" fp)
	    (write-line cod_ident fp)
	    (write-line dim_ident fp)
	    (write-line ang_ident fp)
	    (setq ss_id	nil
		  ss_line nil
	    )
;;;;; proverka za strelka
	    (setq ss_line
		   (ssget "F"
			  ptlist
			  (list (cons 0 "LINE") (cons 8 la-strelka))
		   )
	    )
	    (if	(/= ss_line nil)
	      (progn
		(setq line_id (entget (ssname ss_line 0)))
		(setq pt (cdr (assoc 10 line_id)))
		(setq ss_txt nil)
		(setq ss_txt (ssget "X"
				    (list (cons 0 "TEXT")
					  (cons 10 pt)
					  (cons 8 la-num)
				    )
			     )
		)
		(setq zz (cdr (assoc 1 (entget (ssname ss_txt 0)))))
		(if (= zz cod_ident)
		  (vertex_l line_id)
		  (progn
		    (vertex ent_id)
		    (vertex ent_id)
		  )
		)
	      )
	      (progn
		(vertex ent_id)
		(vertex ent_id)
	      )
	    )
	    (setq all_vert_ins (reverse all_vert_ins))
	    (setq vert_ins (reverse vert_ins))
	    (write-line (r4 (itoa br_inside)) fp)
	    (write-line (r4 (itoa br_vert)) fp)
	    (if	(/= br_inside 0)
	      (progn
		(foreach x all_vert_ins (write-line (r4 (itoa x)) fp))
	      )
	    )
	    (vertex ent)
	    (if	(/= br_inside 0)
	      (foreach x vert_ins
		(write-line (r12 (rtos x 2 3)) fp)
	      )
	    )
	  )
	)
	(setq i (+ i 1))
      )
      (close fp)
    )
  )
  (princ (strcat "\n" la-part " - " (itoa broi)))
)




(defun raster-out (/ ss i broi ent cod vert pt ss1 ent1 name)
  (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 8 "RASTER") (cons 70 1))))
  (setq broi 0)
  (if ss
    (progn
      (setq i	0
	    ss1	nil
      )
      (setq broi (sslength ss))
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq pt (cdr (assoc 10 ent)))
	(setq
	  ss1 (ssget
		"X"
		(list (cons 0 "TEXT") (cons 8 "RASTER") (cons 10 pt))
	      )
	)
	(if ss1
	  (progn
	    (setq ent1 (entget (ssname ss1 0)))
	    (setq name (cdr (assoc 1 ent1)))
	  )
	  (progn
	    (setq name "MANCA NOME")
	    (show_error pt)
	  )
	)
	(setq vert (cdr (assoc 90 ent)))
	(setq vert (r4 (itoa vert)))
	(setq fp (open map "a"))
	(write-line "RIFERIMENTO_RASTER" fp)
	(write-line name fp)
	(vertex ent)
	(close fp)
	(setq i (+ i 1))
      )
    )
  )
  (princ (strcat "\nRASTER - " (itoa broi)))
)

(defun lib-out (/ ss i broi ent vert)
  (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 8 "LIBRETTO"))))
  (setq broi 0)
  (if ss
    (progn
      (setq i 0)
      (setq broi (sslength ss))
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq vert (cdr (assoc 90 ent)))
	(setq vert (r4 (itoa vert)))
	(setq fp (open map "a"))
	(write-line "LIBRETTO" fp)
	(write-line "0" fp)
	(write-line "   1" fp)
	(write-line vert fp)
	(vertex ent)
	(close fp)
	(setq i (+ i 1))
      )
    )
  )
  (princ (strcat "\nLIBRETTO - " (itoa broi)))
)














(defun lin-out (la-lin / ss i broi ent cod vert)
  (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 8 la-lin))))
  (setq broi 0)
  (if ss
    (progn
      (setq i 0)
      (setq broi (sslength ss))
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq cod (cdr (assoc 6 ent)))
	(setq cod (r4 cod))
	(setq vert (cdr (assoc 90 ent)))
	(setq vert (r4 (itoa vert)))
	(if (= la-lin "LINEA-OUT")
	  (setq la-lin "LINEA\\")
	)
	(setq fp (open map "a"))
	(write-line la-lin fp)
	(write-line cod fp)
	(write-line vert fp)
	(vertex ent)
	(close fp)
	(setq i (+ i 1))
      )
    )
    (progn
      (if (= la-lin "LINEA-OUT")
	(setq la-lin "LINEA\\")
      )
      (princ (strcat "\n" la-lin " - " (itoa broi)))
    )
  )
  (princ (strcat "\n" la-lin " - " (itoa broi)))
)

(defun fidu-out
       (la-fidu / ss broi ent cod pt1 pt2 ss1 ent1 ss2 ent2 num)
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 8 la-fidu))))
  (setq broi 0)
  (if (= ss nil)
    (progn
      (if (= la-fidu "FIDUCIALE-OUT")
	(setq la-fidu "FIDUCIALE\\")
      )
      (princ (strcat "\n" la-fidu " - " (itoa broi)))
    )
    (progn
      (setq i 0)
      (setq broi (sslength ss))
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq cod (cdr (assoc 2 ent)))
	(setq cod (r4 cod))
	(setq pt1 (cdr (assoc 10 ent)))
	(setq ss1 (ssget "X" (list (cons 0 "LINE") (cons 8 la-fidu) (cons 10 pt1))))
	(if ss1
	  (progn
	    (setq ent1 (entget (ssname ss1 0)))
	    (setq pt2 (cdr (assoc 11 ent1)))
	    (setq ss2 (ssget "X" (list (cons 0 "TEXT") (cons 10 pt2))))
	    (if	ss2
	      (progn
		(setq ent2 (entget (ssname ss2 0)))
		(setq num (cdr (assoc 1 ent2)))
		(setq pt1x (rtos (car pt1) 2 3))
		(setq pt1y (rtos (cadr pt1) 2 3))
		(setq pt2x (rtos (car pt2) 2 3))
		(setq pt2y (rtos (cadr pt2) 2 3))
		(if (= la-fidu "FIDUCIALE-OUT")
		  (setq la-fidu "FIDUCIALE\\")
		)
		(setq fp (open map "a"))
		(write-line la-fidu fp)
		(write-line num fp)
		(write-line cod fp)
		(write-line (r12 pt1x) fp)
		(write-line (r12 pt1y) fp)
		(write-line (r12 pt2x) fp)
		(write-line (r12 pt2y) fp)
		(close fp)
	      )
	      (progn
		(command "circle" pt2 "50")
		(alert "Problem FIDUCIALE")
		(exit)
	      )
	    )
	  )
	  (progn
	    (command "circle" pt1 "50")
	    (alert "Problem FIDUCIALE")
	    (exit)
	  )
	)
	(setq i (+ i 1))
      )
      (princ (strcat "\n" la-fidu " - " (itoa broi)))
    )
  )
)


(defun sim-out (la-sim / cod ss i broi ent ang)
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 8 la-sim))))
  (setq broi 0)
  (if (= ss nil)
    (progn
      (if (= la-sim "SIMBOLO-OUT")
	(setq la-sim "SIMBOLO\\")
      )
      (princ (strcat "\n" la-sim " - " (itoa broi)))
    )
    (progn
      (setq i 0)
      (setq broi (sslength ss))
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq cod (cdr (assoc 2 ent)))
	(setq cod (r4 cod))
	(setq ang (rtos (cdr (assoc 50 ent)) 2 3))
	(setq ang (r12 ang))
	(if (= la-sim "SIMBOLO-OUT")
	  (setq la-sim "SIMBOLO\\")
	)
	(setq fp (open map "a"))
	(write-line la-sim fp)
	(write-line cod fp)
	(write-line ang fp)
	(vertex ent)
	(close fp)
	(setq i (+ i 1))
      )
      (princ (strcat "\n" la-sim " - " (itoa broi)))
    )
  )
)


(defun text-out	(la-txt / ss i broi ent txt dim ang fp)
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 la-txt))))
  (setq broi 0)
  (if (= la-txt "TESTO-OUT")
    (setq la-txt "TESTO\\")
  )

  (if (= ss nil)
    (princ (strcat "\n" la-txt " - " (itoa broi)))
    (progn
      (setq i 0)
      (setq broi (sslength ss))
      (while (< i broi)
	(setq ent (entget (ssname ss i)))
	(setq txt (cdr (assoc 1 ent)))
	(setq dim (cdr (assoc 40 ent)))
	(setq dim (/ dim scala))
	(setq dim (rtos (* dim 10000) 2 0))
	(setq dim (r4 dim))
	(setq ang (rtos (cdr (assoc 50 ent)) 2 3))
	(setq ang (r12 ang))
	(setq fp (open map "a"))
	(write-line la-txt fp)
	(write-line txt fp)
	(write-line dim fp)
	(write-line ang fp)
	(vertex ent)
	(close fp)
	(setq i (+ i 1))
      )
      (progn
	(if (= la-txt "TESTO-OUT")
	  (setq la-txt "TESTO\\")
	)
	(princ (strcat "\n" la-txt " - " (itoa broi)))
      )
    )
  )
)





;;;;; Dopalnitelni funkcii !!!!!
(defun vertex (obj / a)
  (foreach a obj
    (if	(= 10 (car a))
      (progn
	(setq vertxy (cdr a))
	(setq vertx (rtos (car vertxy) 2 3))
	(setq verty (rtos (cadr vertxy) 2 3))
	(write-line (r12 vertx) fp)
	(write-line (r12 verty) fp)
      )
    )
  )
)


(defun vertex_l	(obj / a vertxy vertx10 vertx11 verty11 verty10)
  (foreach a obj
    (if	(= 10 (car a))
      (progn
	(setq vertxy (cdr a))
	(setq vertx10 (rtos (car vertxy) 2 3))
	(setq verty10 (rtos (cadr vertxy) 2 3))
      )
    )
    (if	(= 11 (car a))
      (progn
	(setq vertxy (cdr a))
	(setq vertx11 (rtos (car vertxy) 2 3))
	(setq verty11 (rtos (cadr vertxy) 2 3))
      )
    )
  )
  (write-line (r12 vertx11) fp)
  (write-line (r12 verty11) fp)
  (write-line (r12 vertx10) fp)
  (write-line (r12 verty10) fp)

)

(defun r4 (txt4 / a)
  (while (< (strlen txt4) 4)
    (setq txt4 (strcat " " txt4))
  )
  (setq a txt4)
)

(defun r12 (txt12 / a)
  (while (< (strlen txt12) 12)
    (setq txt12 (strcat " " txt12))
  )
  (setq a txt12)  
)

(defun show_error (pt /)
  (entmake (list (cons 0 "CIRCLE")
		 (cons 8 "ERROR")
		 (cons 10 pt)
		 (cons 40 50)
	   )
  )
)




