(defun sup-out (/	   map	    map1     date     n-part	n-str	 n-acq
		  n-pol    ss obj  ar-tot
		  ar-str   ar-acq   ar-part  ar-polig tip	fp	 x
		  y i
		 )
  (setq map1 (substr (getvar "DWGNAME") 1 11))
  (setq map (strcat (getvar "DWGPREFIX") map1 ".sup"))
  (setq	n-part 0
	ar-polig 0
	ar-part	0
	ar-str 0
	ar-acq 0
	n-str 0
	n-acq 0
	i 0
  )
  (if (= ar-svil nil) (setq ar-svil 0))
  (setq date (rtos (getvar "CDATE") 2 0))
  (setq sp-pol (vl-sort sp-pol (function (lambda (x y) (< (car x) (car y))))))
  (foreach x sp-pol
    (setq tip (car x))
    (setq ar-polig (cdr x))
    (if	(= tip "STRADA")
      (progn
	(setq n-str (+ n-str 1))
;	(setq ar-polig (atoi (rtos ar-polig 2 0)))
	(setq ar-str (+ ar-str ar-polig))
      )
    )
    (if	(= tip "ACQUA")
      (progn
	(setq n-acq (+ n-acq 1))
;	(setq ar-polig (atoi (rtos ar-polig 2 0)))
	(setq ar-acq (+ ar-acq ar-polig))
      )
    )
    (if	(and (/= tip "ACQUA") (/= tip "STRADA"))
      (progn
	(setq n-part (+ n-part 1))
	(setq ar-part (+ ar-part ar-polig))
      )
    )
  )
  (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 8 "CONFINE") (cons 70 1))))
  (setq n-conf (sslength ss))
  
  (setq fp (open map "w"))
  (write-line (strcat map1 (sup10 date)) fp)
  (write-line (strcat "N.FABBRIC  " (sup10 (itoa n-edi))) fp)
  (write-line (strcat "N.PARTIC   " (sup10 (itoa n-part))) fp)
  (write-line (strcat "N.STRADE   " (sup10 (itoa n-str))) fp)
  (write-line (strcat "N.ACQUE    " (sup10 (itoa n-acq))) fp)
  (write-line (strcat "N.SVIL-ALL " (sup10 (itoa n-svil))) fp)
  (write-line (strcat "FABBRIC    " (sup10 (rtos ar-edi 2 0))) fp)
  (foreach x sp-pol
    (setq tip (car x))
    (setq ar-polig (cdr x))
    (if	(and (/= tip "ACQUA") (/= tip "STRADA"))
      (write-line (strcat (sup11 tip) (sup10 (rtos ar-polig 2 0))) fp)
    )
  )
  (write-line (strcat "PARTIC     " (sup10 (rtos ar-part 2 0))) fp)
  (write-line (strcat "STRADE     " (sup10 (rtos ar-str 2 0))) fp)
  (write-line (strcat "ACQUE      " (sup10 (rtos ar-acq 2 0))) fp)
  (write-line (strcat "SVIL-ALL   " (sup10 (rtos ar-svil 2 0))) fp)
  (setq ar-tot (+ ar-acq ar-str ar-part ar-svil))
  (write-line (strcat "TOTALE     " (sup10 (rtos ar-tot 2 0))) fp)
  (write-line (strcat "CONFINE    " (sup10 (rtos ar-conf 2 0))) fp)
  (write-line (strcat "SBILANCIO  " (sup10 (rtos (- ar-tot ar-conf) 2 0))) fp)
  (close fp)
)

(defun ar-pol (obj / ar)
  (command ".area" "o" obj)
  (setq ar (getvar "AREA"))
)

(defun sup11 (txt /)
  (if (> (strlen txt) 11)
    (progn
      (alert
	(strcat "Greshka!!! Tekst s poveche ot 11 simvola - " txt)
      )
      (exit)
    )
  )
  (while (< (strlen txt) 11)
    (setq txt (strcat txt " "))
  )
)

(defun sup10 (txt /)
  (if (> (strlen txt) 10)
    (progn
      (alert
	(strcat "Greshka!!! Tekst s poveche ot 11 simvola - " txt)
      )
      (exit)
    )
  )
  (while (< (strlen txt) 10)
    (setq txt (strcat " " txt))
  )
)