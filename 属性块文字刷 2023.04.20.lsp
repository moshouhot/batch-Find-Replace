;***************************************************************************
;;;属性块文字刷，中南市政院二院Tiger和gpt开发
;;;***************************************************************************

;; 定义 bgg 命令函数
(defun c:bgg ()
  (vl-load-com) ; 加载 Visual LISP 函数库
  ;; 选择源属性块
  (setq sourceBlk (entsel "\n选择源属性块："))
  (if sourceBlk
    (progn
      ;; 获取所有的插入图块
      (setq ss1 (ssget '((0 . "INSERT"))))
      (setq ss1_sslen (sslength ss1))
      (setq ssn 0)
      ;; 遍历所有的插入图块
      (while (< ssn ss1_sslen)
        (setq en1 (ssname ss1 ssn))
        ;; 检查当前遍历的图块是否与源属性块相同
        (if (= (cdr (assoc 2 (entget en1))) (cdr (assoc 2 (entget (car sourceBlk)))))
          ;; 复制属性设置
          (copy-attribute-settings (car sourceBlk) en1)
        )
        (setq ssn (+ 1 ssn))
      ))
  )
  (princ) ; 退出函数并显示结果
)

;; 更新单个属性值的函数
(defun OAPUT_ONEATT_ZBV (EN ATTNAME int VALUE / RETURN E TEST ENT)
  ;; 初始化变量
  (setq E EN
        RETURN NIL
        TEST t
  )
  ;; 遍历实体的属性
  (while (and TEST (setq E (entnext E)))
    (setq ENT (entget E))
    (cond
      ;; 如果实体不是 ATTRIB，则终止循环
      ((not (= (cdr (assoc 0 ENT)) "ATTRIB"))
       (setq TEST NIL)
      )
      ;; 如果实体是 SEQEND，则终止循环
      ((= "SEQEND" (cdr (assoc 0 ENT)))
       (setq TEST NIL)
      )
      ;; 如果找到需要更新的属性
      ((= (cdr (assoc 2 ENT)) ATTNAME)
        (if (assoc int ENT)
            (progn
              (setq ENT (subst (cons int VALUE)(assoc int ENT) ENT))
              (entmod ENT)
              (entupd EN)
              (setq RETURN t)
             )
           (if VALUE
             (progn
               (setq ENT (cons (cons int VALUE) ENT))
               (entmod ENT)
               (entupd EN)
               (setq RETURN t)
             ))
         ))
      ) ; 结束cond
    )
  ;; 返回结果
  RETURN
)

;; 获取属性块中的属性名称和属性值的函数
(defun get-attribute-names-and-values (blk / attList attEnt attName attData)
  (setq attList nil)
  (if (= (cdr (assoc 0 (entget blk))) "INSERT")
    (progn
      (setq attEnt (entnext blk))
      (while (and attEnt (= (cdr (assoc 0 (entget attEnt))) "ATTRIB"))
        (progn
          (setq attName (cdr (assoc 2 (entget attEnt))))
          (setq attData (list
                         (assoc 7 (entget attEnt))
                         (assoc 8 (entget attEnt))
                         (assoc 40 (entget attEnt))
                         (assoc 41 (entget attEnt))
                         ))
          (setq attList (cons (cons attName attData) attList))
          (setq attEnt (entnext attEnt))
        ))
      (reverse attList)
    ))
)

;; 复制属性设置的函数
(defun copy-attribute-settings (srcBlk trgBlk / srcAttList trgAttList)
  (setq srcAttList (get-attribute-names-and-values srcBlk))
  (setq trgAttList (get-attribute-names-and-values trgBlk))
  (foreach srcAtt srcAttList
    (setq srcAttName (car srcAtt))
    (setq srcAttData (list
                      (assoc 7 (cdr srcAtt))
                      (assoc 8 (cdr srcAtt))
                      (assoc 40 (cdr srcAtt))
                      (assoc 41 (cdr srcAtt))
                      ))
    (foreach trgAtt trgAttList
      (setq trgAttName (car trgAtt))
      (if (= srcAttName trgAttName)
        (progn
          (foreach srcItem srcAttData
            (OAPUT_ONEATT_ZBV trgBlk trgAttName (car srcItem) (if srcItem (cdr srcItem) nil))
            ))
        ))
    ))
(princ "\n\tBGG Command Loaded; written by Tiger")
(princ)

