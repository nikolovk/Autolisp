;;;;; funkcia za naprava na spisak sas skalite na kartite
(defun s::startup (/ map ss num dwg-name tip svi ee cen)
  (setvar "FILEDIA" 0)
  (setvar "CMDDIA" 0)
  (setvar "SDI" 1)
  (setq broi 0)
  (setq map (substr (getvar "DWGNAME") 1 11))
  (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 "N-PART"))))
  (if ss
    (setq broi (sslength ss))
  )

  (setq fp (open "broi.txt" "a"))

  (c:check-dwg)
  (setq svi (substr map 11 1))
  (if (/= svi "0")
    (setq tip 1)
  )
  (setq ee "0")
  (if (= tip 1)
    (setq ee "0")
    (progn
      (setq cen (atoi PopraveniGreshkiCEN))
      (if (<= cen 10)
	(setq ee "3.5")
      )
      (if (and (<= cen 20) (> cen 10))
	(setq ee "5")
      )
      (if (and (<= cen 30) (> cen 20))
	(setq ee "8")
      )
      (if (and (<= cen 40) (> cen 30))
	(setq ee "10")
      )
      (if (and (<= cen 60) (> cen 40))
	(setq ee "12")
      )
      (if (and (<= cen 100) (> cen 60))
	(setq ee "15")
      )
      (if (> cen 100)
	(setq ee "18")
      )
    )
  )
  (write-line
    (strcat map
	    " "
	    (itoa broi)
	    " "
	    VsichkiGreshki
	    " "
	    PopraveniGreshki
	    " "
	    VsichkiGreshkiCEN
	    " "
	    PopraveniGreshkiCEN
	    " "
	    ee
    )
    fp
  )

  (close fp)

  (setq fp (open "number.txt" "r"))
  (setq num (atoi (read-line fp)))
  (close fp)

  (setq fp (open "maps.txt" "r"))
  (repeat num
    (setq dwg-name (read-line fp))
  )
  (close fp)
  (close fp)
  (if (= dwg-name nil)
    (progn
      (setvar "FILEDIA" 1)
      (setvar "CMDDIA" 1)
      (setvar "SDI" 0)
      (command ".qsave")
      (command ".quit")
    )
  )
  (setq fp (open "number.txt" "w"))
  (setq num (+ num 1))
  (write-line (itoa num) fp)
  (command ".qsave")
  (command ".open" dwg-name)
)
