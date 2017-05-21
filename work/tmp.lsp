(defun check-vp-pol (/ ss i broi error)
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