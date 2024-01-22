;;; ______________________________________
;;; 名称：递增刷(框选)
;;; 功能：刷文本末尾或首部的数字递增指定值
;;; 命令：dzs       langjs    2012.2.27
;;; ______________________________________
(defun c:dzs (/ a b bb box c co col cv dbak dcl_re dclname e e1 e2 en ent ent1 errsub filen fun i key kk l l1 len lst lst1 lst2 lst3
	       msg name newlist orerr pix pt pt1 pt2 s sbak si ss ss1 ssparm st2 st3 st4 stream tempname txt1 txt2 txt3 txtlong vvs
	       wbak x y zz
	    )
  (defun llt:match (pt col ssparm fun / a b c co cv e e1 e2 ent i len lst msg pix pt1 pt2 si ss ss1 x y) ; 带有刷子的ssget功能子程序
				       ; ,caoyin老大明经“拜新年“帖子提供,langjs修改
    (defun d_brush (col x y len / a b c)
      (grvecs (list col (list (- x (setq a (* len 1.5))) (- y len)) (list (- x a) (setq b (- y (* len 7.5)))) col (list (- x
															   (setq c
																 (* len 0.5)
															   )
															) y
														  ) (list (- x c) b)
		    col (list (+ x c) y) (list (+ x c) b) col (list (+ x a) (- y len)) (list (+ x a) b) col (list (- x (setq a
															     (* len
																4.5
															     )
														       )
														  ) b
													    ) (list (+ x a) b) col
		    (list (- x a) b) (list (- x (setq c (* len 6.5))) (- y (* len 9))) col (list (+ x a) b) (list (+ x c)
														  (setq a (- y
															     (* len 9)
															  )
														  )
													    ) col (list (- x c) a)
		    (list (- x c) (setq b (- y (* len 17)))) col (list (+ x c) a) (list (+ x c) b) col (list (- x c) (setq a
															   (- y
															      (* len
																 10
															      )
															   )
														     )
												       ) (list (+ x c) a) col
		    (list (- x c) (setq a (- y (* len 11)))) (list (+ x c) a) col (list (- x c) (setq a (- y (* len 13))))
		    (list (+ x c) a) col (list (- x c) (setq a (- y (* len 14)))) (list (+ x c) a) col (list (- x c) b)
		    (list (+ x c) b) col (list (- x c) b) (list (- x (* len 11)) (setq a (- y (* len 21.5)))) col (list (- x
															   (* len 2)
															) b
														  ) (list (- x
															     (* len
																6.5
															     )
															  ) a
														    ) col
		    (list (+ x (* len 2)) b) (list (- x (* len 2.5)) a) col (list (+ x c) b) (list (+ x (* len 2)) a) col
		    (list (- x (* len 11)) a) (list (+ x (* len 3)) a)
	      ) (list (list 1 0 0 (* len 14)) (list 0 1 0 (* len -4)) '(0 0 1 0) '(0 0 0 1))
      )
    )
    (defun pickbox (pt / si cv)
      (setq si (* (/ (getvar "pickbox") (cadr (getvar "screensize"))) (getvar "viewsize") 0.5)
	    cv (list si si 0)
      )
      (list (mapcar
	      '+
	      pt
	      cv
	    ) (mapcar
		'-
		pt
		cv
	      )
      )
    )
    (defun p2u (pix)
      (* pix (/ (getvar "viewsize") (cadr (getvar "screensize"))))
    )
    (or
      (setq co (cadr col))
      (setq co 7)
    )
    (or
      (setq col (car col))
      (setq col 7)
    )
    (or
      (setq msg (car ssparm))
      (setq msg "\n选择目标对象: ")
    )
    (setq ssparm (cadr ssparm)
	  len (p2u 1)
	  x (car pt)
	  y (cadr pt)
    )
    (princ msg)
    (while (/= (car pt1) 11)
      (redraw)
      (d_brush col x y len)
      (while (not (member (car (setq pt1 (grread t 12 2))) '(3 11)))
	(setq pt1 (cadr pt1))
	(if (vl-consp pt1)
	  (progn
	    (if (> (distance pt1 pt) (p2u (* 0.0001 (car (getvar "screensize")))))
	      (progn
		(redraw)
		(setq len (p2u 1)
		      x (car pt)
		      y (cadr pt)
		)
		(d_brush col x y len)
		(setq pt pt1)
	      )
	    )
	  )
	)
      )
      (redraw)
      (if (and
	    (= (car pt1) 3)
	    (princ msg)
	    (not (setq ss1 (apply
			     'ssget
			     (append
			       '("_c")
			       (pickbox (cadr pt1))
			       (list ssparm)
			     )
			   )
		 )
	    )
	  )
	(progn
	  (princ "指定对角点: ")
	  (setq pt1 (list (caadr pt1) (cadadr pt1)))
	  (while (not (member (car (setq pt2 (grread t 12 1))) '(3 11)))
	    (setq pt2 (list (caadr pt2) (cadadr pt2)))
	    (if (vl-consp pt1)
	      (progn
		(if (> (distance pt2 pt) (p2u (* 0.0001 (car (getvar "screensize")))))
		  (progn
		    (redraw)
		    (setq len (p2u 1)
			  x (car pt)
			  y (cadr pt)
			  co (abs co)
		    )
		    (if (> (car pt1) (car pt2))
		      (setq co (- co))
		    )
		    (d_brush col x y len)
		    (grvecs (list co pt1 (list (car pt1) (cadr pt2)) co pt2 (list (car pt1) (cadr pt2)) co pt2 (list (car pt2)
														     (cadr pt1)
													       ) co pt1
				  (list (car pt2) (cadr pt1))
			    )
		    )
		    (setq pt pt2
			  ss1 (ssget (if (minusp co)
				       "_c"
				       "_w"
				     ) pt1 pt2 ssparm
			      )
		    )
		  )
		)
	      )
	    )
	  )
	)
      )
      (or
	ss
	(setq ss (ssadd))
      )
      (if ss1
	(progn
	  (setq lst '())
	  (repeat (setq i (sslength ss1))
	    (setq e (ssname ss1 (setq i (1- i))))
	    (setq ent (entget e))
	    (setq pt1 (cdr (assoc 10 ent)))
	    (setq lst (cons (list pt1 e) lst))
	  )
	  (setq lst (vl-sort lst (function (lambda (e1 e2) ; 框选文本先由上到下排序(其它情况自己修改)
					     (> (cadr (car e1)) (cadr (car e2)))
					   )
				 )
		    )
	  )
	  (setq lst (vl-sort lst (function (lambda (e1 e2) ; 框选文本后由左到右排序(其它情况自己修改)
					     (< (car (car e1)) (if (and
								     (>= (car (car e2)) (- (car (car e1)) txtlong))
								     (<= (car (car e2)) (+ (car (car e1)) txtlong))
								   )
								 (car (car e1))
								 (car (car e2))
							       )
					     )
					   )
				 )
		    )
	  )
	  (setq lst (reverse lst))
	  (repeat (setq i (length lst))
	    (setq e (cadr (nth (setq i (1- i))
			       lst
			  )
		    )
	    )
	    (ssadd e ss)
	    (redraw e 3)
	    (apply
	      fun
	      (list e)
	    )
	  )
	)
      )
      (setq ss1 nil)
    )
    (redraw)
    ss
  )
  (defun llt:entsel (pt col / a b c col en ent len pix pt x y) ; 带有刷子的entsel功能子程序,caoyin老大提供
    (defun p2u (pix)
      (* pix (/ (getvar "viewsize") (cadr (getvar "screensize"))))
    )
    (defun d_brush (col x y len / a b c)
      (grvecs (list col (list (- x (setq a (* len 1.5))) (- y len)) (list (- x a) (setq b (- y (* len 7.5)))) col (list (- x
															   (setq c
																 (* len 0.5)
															   )
															) y
														  ) (list (- x c) b)
		    col (list (+ x c) y) (list (+ x c) b) col (list (+ x a) (- y len)) (list (+ x a) b) col (list (- x (setq a
															     (* len
																4.5
															     )
														       )
														  ) b
													    ) (list (+ x a) b) col
		    (list (- x a) b) (list (- x (setq c (* len 6.5))) (- y (* len 9))) col (list (+ x a) b) (list (+ x c)
														  (setq a (- y
															     (* len 9)
															  )
														  )
													    ) col (list (- x c) a)
		    (list (- x c) (setq b (- y (* len 17)))) col (list (+ x c) a) (list (+ x c) b) col (list (- x c) (setq a
															   (- y
															      (* len
																 10
															      )
															   )
														     )
												       ) (list (+ x c) a) col
		    (list (- x c) (setq a (- y (* len 11)))) (list (+ x c) a) col (list (- x c) (setq a (- y (* len 13))))
		    (list (+ x c) a) col (list (- x c) (setq a (- y (* len 14)))) (list (+ x c) a) col (list (- x c) b)
		    (list (+ x c) b) col (list (- x c) b) (list (- x (* len 11)) (setq a (- y (* len 21.5)))) col (list (- x
															   (* len 2)
															) b
														  ) (list (- x
															     (* len
																6.5
															     )
															  ) a
														    ) col
		    (list (+ x (* len 2)) b) (list (- x (* len 2.5)) a) col (list (+ x c) b) (list (+ x (* len 2)) a) col
		    (list (- x (* len 11)) a) (list (+ x (* len 3)) a)
	      ) (list (list 1 0 0 (* len 14)) (list 0 1 0 (* len -4)) '(0 0 1 0) '(0 0 0 1))
      )
    )
    (redraw)
    (setq len (p2u 1)
	  x (car pt)
	  y (cadr pt)
    )
    (d_brush col x y len)
    (while (not (member (car (setq pt1 (grread t 12 2))) '(3 11)))
      (setq pt1 (cadr pt1))
      (if (vl-consp pt1)
	(progn
	  (or
	    pt
	    (setq pt pt1)
	  )
	  (setq x (car pt)
		y (cadr pt)
	  )
	  (if (> (distance pt1 pt) (p2u (* 0.0001 (car (getvar "screensize")))))
	    (progn
	      (redraw)
	      (setq len (p2u 1)
		    x (car pt)
		    y (cadr pt)
	      )
	      (d_brush col x y len)
	      (setq pt pt1)
	    )
	  )
	)
      )
    )
    (redraw)
    (and
      (= (car pt1) 3)
      (vl-consp (cadr pt1))
      (setq en (nentselp (cadr pt1)))
    )
    (setq ent (car en))
    (setq pt1 (cadr pt1))
    (if (cadddr en)
      (setq name (cadddr en))
    )
    ent
  )
  (defun errsub (msg)
    (if (not (member msg '("console break" "Function cancelled"
		      "quit / exit abort" ""
		     )
	     )
	)
      (princ (strcat "\n" msg))
    )				       ; if
    (redraw)
    (setq *error* orerr)
    (prin1)
  )
  (defun show_list (key newlist)
    (start_list key)
    (mapcar
      'add_list
      newlist
    )
    (end_list)
  )
  (defun jys001 (x kk / ent txt1 txt3 zz) ; 首部框
    (setq ent (entget x))
    (setq txt1 (cdr (assoc 1 ent)))
    (setq zz 1)
    (while (or
	     (> (atoi (substr txt1 zz 1)) 0)
	     (= (substr txt1 zz 1) "0")
	   )
      (setq zz (+ zz 1))
    )
    (if (> zz (strlen txt1))
      (setq txt3 kk)
      (setq txt3 (strcat kk (substr txt1 zz)))
    )
    (entmod (subst
	      (cons 1 txt3)
	      (assoc 1 ent)
	      ent
	    )
    )
  )
  (defun jys002 (x / ent txt3)	       ; 尾部框
    (if (and
	  (setq ent (entget x))
	  (member (cdr (assoc 0 ent)) '("TEXT" "MTEXT"))
	)
      (progn
	(setq kk (jys003 kk sbak))
	(if (= zz 0)
	  (setq txt3 kk)
	  (setq txt3 (strcat txt2 kk))
	)
	(entmod (subst
		  (cons 1 txt3)
		  (assoc 1 ent)
		  ent
		)
	)
      )
    )
  )
  (defun jys003 (st2 sbak / l l1 s st3 st4) ; 处理尾部数字
    (setq st3 st2
	  s "0"
	  st4 ""
	  l (strlen st2)
    )
    (while (eq s "0")
      (setq s (substr st3 1 1)
	    st3 (substr st3 2)
	    st4 (if (eq s "0")
		  (strcat st4 s)
		  st4
		)
      )
    )				       ; 将首位是0的数字分离出来
    (setq st2 (atoi st2)
	  st2 (+ st2 sbak)
	  st2 (itoa st2)
	  st2 (strcat st4 st2)
	  l1 (strlen st2)
	  s (substr st2 1 1)
    )
    (if (and
	  (= s "0")
	  (> l1 l)
	)
      (setq st2 (substr st2 2))
    )				       ; 处理了形如A09变为A10的问题
    st2
  )
  (defun dzs001 (lst vvs / e)
    (setq e (nth (atoi vvs) lst))
    (cons e (vl-remove e lst))
  )
  (defun dzs002 ()
    (show_list "e01" lst1)
    (show_list "e02" lst2)
    (show_list "e03" lst3)
  )
  (setvar "cmdecho" 0)
  (vl-load-com)
  (setq orerr *error*)
  (setq *error* errsub)
  (while (progn
	   (if (setq wbak (vl-registry-read "HKEY_CURRENT_USER\\software\\TH++" "wbak"))
	     (setq lst1 (cons wbak (vl-remove wbak '("尾部" "首部"))))
	     (setq lst1 '("尾部" "首部"))
	   )
	   (if (setq sbak (vl-registry-read "HKEY_CURRENT_USER\\software\\TH++" "sbak"))
	     (setq lst2 (cons sbak (vl-remove sbak '("1" "2"
					       "3" "4"
					       "5" "6"
					       "7" "8"
					       "9" "10"
					       "15" "20"
					       "30" "40"
					       "50"
					      )
				   )
			)
	     )
	     (setq lst2 '("1" "2"
		    "3" "4"
		    "5" "6"
		    "7" "8"
		    "9" "10"
		    "15" "20"
		    "30" "40"
		    "50"
		   )
	     )
	   )
	   (if (setq dbak (vl-registry-read "HKEY_CURRENT_USER\\software\\TH++" "dbak"))
	     (setq lst3 (cons dbak (vl-remove dbak '("单选" "窗选"))))
	     (setq lst3 '("单选" "窗选"))
	   )
	   (initget "S ")
	   (if (= (setq ent (nentsel (strcat "\n选择起始文字[设置(S)]:<" (car lst1) "加" (setq sbak (car lst2))
					     ">"
				     )
			    )
		  )
		  "S"
	       )
	     (progn
	       (setq dclname (cond
			       ((setq tempname (vl-filename-mktemp "dzs.dcl")
				      filen (open tempname "w")
				)
				 (foreach stream '("\n" "dzs1:dialog {\n"
				    "     label = \"递增刷v3.0\" ;\n" "     :boxed_column {\n"
				    "         label = \"设置\" ;\n" "         :row {\n"
				    "             :text {label = \" 递增位置\" ; }\n"
				    "             :text {label = \"递增步长\" ;  }\n"
				    "             :text {label = \"目标文字\" ;  }\n" "         }\n"
				    "         :row {\n" "             :popup_list { key = \"e01\" ;  edit_width = 7 ; }\n"
				    "             :popup_list { key = \"e02\" ;  edit_width = 7 ; }\n"
				    "             :popup_list { key = \"e03\" ;  edit_width = 7 ; }\n"
				    "         }\n" "     }\n"
				    "     ok_cancel;\n" " }\n"
				   )
				   (princ stream filen)
				 )
				 (close filen)
				 tempname
			       )
			     )
	       )
	       (setq dcl_re (load_dialog dclname))
	       (if (not (new_dialog "dzs1" dcl_re))
		 (exit)
	       )
	       (show_list "e01" lst1)
	       (show_list "e02" lst2)
	       (show_list "e03" lst3)
	       (action_tile "e01" "(setq lst1 (dzs001 lst1  $value))(dzs002) ")	; 位置
	       (action_tile "e02" "(setq lst2 (dzs001 lst2  $value))(dzs002)") ; 步长
	       (action_tile "e03" "(setq lst3 (dzs001 lst3  $value))(dzs002)") ; 对象
	       (action_tile "accept" "(setq dcl_pt (done_dialog 1)) ") ; 确定
	       (setq bb (start_dialog))
	       (if (= bb 1)
		 (progn
		   (vl-registry-write "HKEY_CURRENT_USER\\software\\TH++" "wbak" (car lst1))
		   (vl-registry-write "HKEY_CURRENT_USER\\software\\TH++" "sbak" (car lst2))
		   (vl-registry-write "HKEY_CURRENT_USER\\software\\TH++" "dbak" (car lst3))
		 )
	       )
	       (unload_dialog dcl_re)
	       (vl-file-delete dclname)
	     )
	   )
	   (or
	     (= ent "S")
	     (null ent)
	     (not (member (cdr (assoc 0 (entget (car ent)))) '("TEXT" "MTEXT"
			   "ATTRIB"
			  )
		  )
	     )
	   )
	 )
    (if (= 52 (getvar "errno"))
      (vl-exit-with-error "")
    )
  )
  (setq pt1 (cadr ent))
  (setq txt1 (entget (car ent)))
  (setq box (textbox (list (assoc 1 txt1) (assoc 40 txt1) (assoc 7 txt1)))) ; 文本框坐标
  (setq txtlong (/ (- (car (cadr box)) (car (car box))) 2))
  (setq txt1 (cdr (assoc 1 txt1)))
  (setq sbak (car lst2))
  (setq sbak (atoi sbak))
  (if (= (car lst1) "尾部")
    (progn
      (setq zz (strlen txt1))
      (while (and
	       (>= zz 1)
	       (or
		 (> (atoi (substr txt1 zz 1)) 0)
		 (= (substr txt1 zz 1) "0")
	       )
	     )
	(setq zz (- zz 1))
      )
      (setq kk (substr txt1 (+ zz 1)))
      (if (/= zz 0)
	(setq txt2 (substr txt1 1 zz))
      )
      (command ".UNDO" "BE")
      (if (= (car lst3) "窗选")
	(llt:match pt1 '(2 7) (list (strcat "\n选择目标文字:<尾部加" (itoa sbak) ">") '((0 . "*TEXT"))) '(lambda (x /)
													   (jys002 x)
													 )
	)
	(while t
	  (princ (strcat "\n选择目标文字:<尾部加" (itoa sbak) ">"))
	  (setq name nil)
	  (if (and
		(setq ent (llt:entsel pt1 2))
		(setq ent (entget ent))
		(member (cdr (assoc 0 ent)) '("TEXT" "MTEXT"
			 "ATTRIB"
			)
		)
	      )
	    (progn
	      (setq kk (jys003 kk sbak))
	      (if (= zz 0)
		(setq txt3 kk)
		(setq txt3 (strcat txt2 kk))
	      )
	      (if name
		(if (and
		      (setq ent1 (entget (car name)))
		      (= (cdr (assoc 0 ent1)) "DIMENSION")
		    )
		  (entmod (subst
			    (cons 1 txt3)
			    (assoc 1 ent1)
			    ent1
			  )
		  )
		  (progn
		    (entmod (subst
			      (cons 1 txt3)
			      (assoc 1 ent)
			      ent
			    )
		    )
		    (entupd (car name))
		  )
		)
		(entmod (subst
			  (cons 1 txt3)
			  (assoc 1 ent)
			  ent
			)
		)
	      )
	    )
	    (vl-exit-with-error "")
	  )
	)
      )
      (command ".UNDO" "E")
    )
  )
  (if (= (car lst1) "首部")
    (progn
      (setq zz 1)
      (while (or
	       (> (atoi (substr txt1 zz 1)) 0)
	       (= (substr txt1 zz 1) "0")
	     )
	(setq zz (+ zz 1))
      )
      (if (= zz 1)
	(setq kk "0")
	(setq kk (substr txt1 1 (- zz 1)))
      )
      (command ".UNDO" "BE")
      (if (= (car lst3) "窗选")
	(progn
	  (setq kk (jys003 kk sbak))
	  (llt:match pt1 '(2 7) (list (strcat "\n选择目标文字:<首部加" (itoa sbak) ">") '((0 . "*TEXT"))) '(lambda (x /)
													     (jys001 x kk)
													   )
	  )
	)
	(while t
	  (setq kk (jys003 kk sbak))
	  (princ (strcat "\n选择目标文字:<首部加" (itoa sbak) ">"))
	  (setq name nil)
	  (if (and
		(setq ent (llt:entsel pt1 2))
		(setq ent (entget ent))
		(member (cdr (assoc 0 ent)) '("TEXT" "MTEXT"
			 "ATTRIB"
			)
		)
	      )
	    (progn
	      (setq txt1 (cdr (assoc 1 ent)))
	      (setq zz 1)
	      (while (or
		       (> (atoi (substr txt1 zz 1)) 0)
		       (= (substr txt1 zz 1) "0")
		     )
		(setq zz (+ zz 1))
	      )
	      (if (> zz (strlen txt1))
		(setq txt3 kk)
		(setq txt3 (strcat kk (substr txt1 zz)))
	      )
	      (if name
		(if (and
		      (setq ent1 (entget (car name)))
		      (= (cdr (assoc 0 ent1)) "DIMENSION")
		    )
		  (entmod (subst
			    (cons 1 txt3)
			    (assoc 1 ent1)
			    ent1
			  )
		  )
		  (progn
		    (entmod (subst
			      (cons 1 txt3)
			      (assoc 1 ent)
			      ent
			    )
		    )
		    (entupd (car name))
		  )
		)
		(entmod (subst
			  (cons 1 txt3)
			  (assoc 1 ent)
			  ent
			)
		)
	      )
	    )
	    (vl-exit-with-error "")
	  )
	)
      )
      (command ".UNDO" "E")
    )
  )
  (setq *error* orerr)
  (princ)
)
