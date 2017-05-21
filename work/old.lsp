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
