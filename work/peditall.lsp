
(DEFUN C:PALL( / AA )
(while (= aa nil)
  (SETQ AA (CAR (ENTSEL "\n Select line:")))
 )
  (COMMAND ".PEDIT" aa "" "join" "all" "" "")
  (PRINC "AAAAAAA")
)

(DEFUN C:PAL ( / AA )
(while (= aa nil)
  (SETQ AA (CAR (ENTSEL "\n Select pline:")))
 )
  (COMMAND ".PEDIT" aa "join" "all" "" "")
  (PRINC "AAAAAAA")
)
