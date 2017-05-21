box:dialog { label = "Insert TEXT";

                  : list_box { key   = "list";
                               width = 70;
                               fixed_width = true;
                               label = "List of words";
                  }
                  : edit_box { key   = "edit";
                               edit_width = 60;
                               edit_limit = 80;
//                               label = "Text";
                               allow_accept = true;
                  }
                  :row { alignment = centered;
                    : button { label = "Delete text";
                               key = "del_text";
                               alignment = top;
                               fixed_alignment = true;
                    }
                    ok_cancel_err;
                  }
              }  // end of box definition
