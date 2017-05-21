(defun change_snap ( / p1_snap p2_snap sn_line sn_line_list )
   (setq sn_line (entsel "\n Select line from snap:"))
   (if (/= sn_line nil)
      (progn
         (setq sn_line_list (entget (car sn_line)))
         (setq p1_snap (cdr (assoc 10 sn_line_list)))
         (setq p2_snap (cdr (assoc 11 sn_line_list)))
;        (command "snap" "r" p1_snap p2_snap)
         (command "_snap" "_r" p1_snap p2_snap)
;        (command "snap" 0.00001)
         (command "_snap" 0.00001)
      );end progn
   );end if
);end change_snap

(defun c:sn ()
   (setq pkl (LIST 0 0 0))
;  (command "snap" "r" PKL 0.0)
   (command "_snap" "_r" PKL 0.0)
    (command "snapbase" "0,0")
  (command "snapang" "0")
  (setvar "snapmode" 0)
)


(defun DrawHouse ( la / flag_snap a b sd point_1 point_2 List_point list_command slap
                        flag_line e1 a1);
   (menucmd"S=LineHouse")
;  (command "clayer" la)
   (command "_clayer" la)
   (setq flag_snap nil )
   (setq List_point (list))
   (setq List_command (list))
   (setq point_1 " ")
   (while (and (/= (type point_1) 'list) (/= point_1 nil))
      (initget "CS P ORT REAK U NEP R X E2")
      (setq point_1 (getpoint "\n  From point:" ))
      (if (= (type point_1) 'STR)
         (progn
            (cond
               ((= point_1 "P")
                  (if (= (getvar "clayer") "PARTICELLE")
                      (setvar "clayer" "EDIFICI")
                      (setvar "clayer" "PARTICELLE")
                  );end if
               )
               ((= point_1 "E2")
                  (c:e2)
                  ;(setq point_1 (getvar "lastpoint"))
                  (setvar "clayer" "edifici")
                  (flag_line 1)
               )
               ((= point_1 "ORT")
                  (progn
                     (if (= (getvar "orthomode") 1)
                        (setvar "orthomode" 0)
                        (setvar "orthomode" 1)
                     )
                  );end progn
               )
               ((= point_1 "R") (C:R))
               ((= point_1 "REAK")
                  (PROGN
                     (setq List_command (append '("REAK") List_command))
                     (setq a (entsel "\n- Izberete obekt i tocka:"))
                     (if (/= A Nil)
                        (progn              (setq b (car a))
                           (setq p1 (cadr a))
;                          (command "break" B p1 p1)
                           (command "_break" B p1 p1)
                           (setq sd (entget (entlast)))
                           (setq point_1 (cdr (assoc 10 sd)))
                        );end progn
                     );end if
                  );end progn
               )
            );end cond
         );end progn
      );end if
   );end while
   (if (/= point_1 nil)
      (progn
         (setq List_point (append (list point_1) List_point))
         (setvar "cmdecho" 0)
         (setq POINT_2 " ")
         (setq A1 point_1)
         (while  (/= point_2 NIL)
            (initget "CS ORT REAK P U NEP R X E2")
            (setq point_2 (getpoint point_1 "\n- Do tocka:"))
            (if (= (type point_2) 'STR)
               (progn
                  (cond
                     ((= point_2 "P")
                       (if (= (getvar "clayer") "PARTICELLE")
                           (setvar "clayer" "EDIFICI")
                           (setvar "clayer" "PARTICELLE")
                       );end if
                     )
                     ((= point_2 "E2")
                      (c:e2)
                      ;(setq point_2 (getvar "lastpoint"))
                      (setvar "clayer" "edifici")
                      (setq flag_line 1)
                     )
                     ((= point_2 "ORT")
                        (progn
                           (if (= (getvar "orthomode") 1)
                              (setvar "orthomode" 0)
                              (setvar "orthomode" 1)
                           );end if
                           (setq List_command (append '("ORT") List_command))
                        );end progn
                     )
                     ((= point_2 "CS") (change_snap))
                     ((= point_2 "R") (C:R))
                     ((= point_2 "NEP")
                        (progn
;                          (command "osnap" "per")
                           (command "_osnap" "_per")
;                          (command "ortho" "off")
                           (command "_ortho" "_off")
                           (initget " NEP ")
                           (setq point_2 (getpoint point_1))
;                          (command "osnap" "off")
                           (command "_osnap" "_off")
;                          (command "ortho" "on")
                           (command "_ortho" "_on")
                           (setq List_command (append '("NEP") List_command))
                        );end progn
                     )
                     ((= point_2 "REAK")
                        (progn
                           (setq List_command (append '("REAK") List_command))
                           (setq A (entsel "\n- Izberete obekt i tocka:"))
                           (if (/= A Nil)
                              (progn
                                 (setq b (car a))
                                 (setq point_2 (cadr a))
;                                (command "break" B point_2 point_2)                 (SETQ SD (ENTGET (ENTLAST)))
                                 (command "_break" B point_2 point_2)                 (SETQ SD (ENTGET (ENTLAST)))
                                 (SETQ point_2 (CDR (ASSOC 10 SD)))
                              );end progn
                           );end if
                        );end progn
                     )
                     ((= point_2 "U")
                        (progn
                           (setq commands (car list_command))
                           (if (/= commands nil)
                              (progn
                                 (setq list_command (cdr list_command))
                                 (setq commands1 (car list_command))
                                 (if (= commands1 "REAK")
                                    (progn
;                                      (command "UNDO" 2)
                                       (command "_UNDO" 2)
                                       (setq list_command (cdr list_command))
                                       (setq commands commands1)
                                       (setq List_point (cdr List_point))
                                       (if (/= list_point nil)
                                          (setq point_1 (car List_point))                        (setq point_2 nil)
                                       );end if
                                    );end progn
                                 );end if
                                 (IF (= commands "LINE")
                                    (progn
;                                      (command "UNDO" 3)
                                       (command "_UNDO" 3)
                                       (setq List_point (cdr List_point))
                                       (if (/= list_point nil)
                                          (setq point_1 (car List_point))
                                          (setq point_2 nil)
                                       );end if
                                    );end progn
                                 );end if
                                 (if (= commands "NEP")
                                    (progn
;                                      (command "UNDO" 4)
                                       (command "_UNDO" 4)
                                       (setq List_point (cdr List_point))
                                       (if (/= list_point nil)
                                          (setq point_1 (car List_point))
                                          (setq point_2 nil)
                                       );end if
                                    );end progn
                                 );end IF
                                 (if (and (/= commands "LINE") (/= commands "REAK") (/= commands "R") (/= commands "NEP"))
                                    (progn
;                                      (command "UNDO" 3)
                                       (command "_UNDO" 3)
                                       (c:r)
                                    );end progn
                                 );end if
                              );end progn
                              (setq point_2 nil)
                           );end if
                        );end progn
                     )
                  );end cond
               );end progn
            );end if
            (if (= (type point_2) 'LIST)
               (progn
;                 (command "UNDO" "G")
                  (command "_UNDO" "_G")
                  (if (= flag_snap nil)
                     (progn
                        (cur_snap point_1 point_2)
                        (setq flag_snap 1)
                        ;(if (/= (flag_line POINT_1 POINT_2) 1)
                        (if (= flag_line  nil)
                           (progn
                              (setq List_command (append '("LINE") List_command))
;                             (command "LINE" point_1 point_2 "")
                              (command "_LINE" point_1 point_2 "")
                              (setq flag_line nil)
                           )
                              (setq flag_line nil)
                        )
;                       (command "UNDO" "END")
                        (command "_UNDO" "_END")
                        (setq point_1 point_2)
                        (setq List_point (append (list point_2) List_point))
                     );end progn
                     (progn
                        (setq List_point (append (list point_2) List_point))
;                       (command "UNDO" "G")
                        (command "_UNDO" "_G")
                        ;(if (/= (flag_line POINT_1 POINT_2) 1)
                        (if (= flag_line  nil)
                           (progn
                              (setq List_command (append '("LINE") List_command))
;                             (command "LINE" point_1 point_2 "")
                              (command "_LINE" point_1 point_2 "")
                              (setq flag_line nil)
                           )
                              (setq flag_line nil)
                        )
;                       (command "UNDO" "END")
                        (command "_UNDO" "_END")
                        (setq point_1 point_2)
                     );end progn
                  );end if
               );end progn
            );end if
         );end while
;        (command "ortho" "off")
         (command "_ortho" "_off")
         (SETQ PKL (LIST 0 0 0))
;        (command "snap" "r" PKL 0.0)
         (command "_snap" "_r" PKL 0.0)
;        (command "snap" "off")
         (command "_snap" "_off")
         (setq e2 (entlast))
         (setq sss (entget e2))
         (setq p1 (cdr (assoc 11 sss)))
         (if (equal a1 p1)
            (grread 0)
            (progn
;              (command "line" a1 "per" p1 "")
               (command "_line" a1 "_per" p1 "")
               (setq e3 (entlast))
;              (command "chamfer" "d" 0 0)
               (command "_chamfer" "_d" 0 0)
               ;;;да не ▒е из▓░ижа го░ни┐ ░ед за ┤а▒ки
;              (command "chamfer" e2 e3)
               (command "_chamfer" e2 e3)
            );end progn
         );end if
      );end progn
   );end if
   (menucmd"S=SCREEN")
) ; end DrawHouse


(defun cur_snap ( p1 p2 / )
;  (command "snap" "r" p1 p2 )
   (command "_snap" "_r" p1 p2 )
;  (command "snap" 0.00001)
   (command "_snap" 0.00001)
;  (command "ortho" "on")
   (command "_ortho" "_on")
)
(defun flag_line ( p1 p2 / flag lista_pt select_line len_select_line n )
   (setq flag 0)
   (setq lista_pt (list p1 p2))
   (setq select_line (ssget "FP" lista_pt))
   (if (/= select_line nil)
      (progn
         (setq len_select_line (sslength select_line))
         (setq n 0)
         (repeat len_select_line
            (setq cur_ob (ssname select_line n))
            (setq list_cur_ob (entget cur_ob))
            (if (or (and (equal p1 (cdr (assoc 10 list_cur_ob))) (equal p2 (cdr (assoc 11 list_cur_ob)))) (and (equal p2 (cdr (assoc 10 list_cur_ob))) (equal p1 (cdr (assoc 11 list_cur_ob)))))
               (setq flag 1)
            )
            (setq n (1+ n))
         );end repeat
      );end progn
   );end if
   flag
)
