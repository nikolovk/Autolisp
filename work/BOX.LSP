(defun box ( lst / dcl_id result)
   (setq dcl_id (load_dialog "box.dcl"))
   (if (not (new_dialog "box" dcl_id))  (exit) )

   (setq result "")
   (set_tile "edit" "")
   (start_list "list")
   (setq x 0)
   (repeat (length lst)
     (add_list (nth x lst))
     (setq x(1+ x))
   )
   (end_list)

   (action_tile "list" "(list_func $value $reason)")
   (action_tile "accept" "(do_check_at_ok)")
   (action_tile "del_text" "(del_text)")

   (start_dialog)
   (unload_dialog dcl_id)
   result
) ; end box


(defun list_func (val rea / a b c)
 (if (= (get_tile "edit") "")
    (set_tile "edit" (nth (atoi val) lst))
    (set_tile "edit" (strcat (get_tile "edit") " " (nth (atoi val) lst)))
 )
 (mode_tile "edit" 3)
 (mode_tile "edit" 4)
)


(defun del_text ()
 (set_tile "edit" "")
)


(defun do_check_at_ok ()
 (setq result (get_tile "edit"))
 (done_dialog)
)
