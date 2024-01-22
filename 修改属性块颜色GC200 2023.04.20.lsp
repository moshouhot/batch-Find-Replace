;***************************************************************************
;;;地形图属性块改色，中南市政院二院Tiger和gpt开发
;;;***************************************************************************
(defun set-attribute-color (blkEntity color / attList attEnt attEntData)
  ; 获取属性块的所有属性
  (setq attList (get-attribute-all-names-and-values blkEntity))
  ; 遍历所有属性
  (foreach attPair attList
    (setq attEnt (cdr (assoc -1 attPair)))
    ; 如果属性不包含62组码（颜色），则添加颜色，否则更新颜色
    (setq attEntData (entget attEnt))
    (if (assoc 62 attEntData)
      (setq attEntData (subst (cons 62 color) (assoc 62 attEntData) attEntData))
      (setq attEntData (append attEntData (list (cons 62 color))))
    )
    (entmod attEntData)
  )
)

(defun c:setcolor ()
  (princ "\n选择要修改颜色的属性块（默认只选GC200）:")
  ; 选择图层内的所有实体
  (setq ss (ssget '((0 . "*"))))
  (setq color (getint "\n请输入颜色值 (默认8): "))
  (if (= color nil) (setq color 8)) ; 如果用户未输入颜色，则使用默认颜色1
  (setq i 0 gc200-count 0 total-count 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (if (= "INSERT" (cdr (assoc 0 (entget ent))))
        (progn
          (setq total-count (+ total-count 1))
          (if (= "GC200" (cdr (assoc 2 (entget ent))))
            (progn
              (setq gc200-count (+ gc200-count 1))
              (set-attribute-color ent color)
            )
          )
        )
    )
    (setq i (+ i 1))
  )
  (princ (strcat "\n总共选择了 " (itoa total-count) " 个属性块，其中 GC200 块有 " (itoa gc200-count) " 个。"))
  (princ)
)

; 获取属性块的所有属性
(defun get-attribute-all-names-and-values (blk / attList attEnt)
  (setq attList nil)
  (if (= (cdr (assoc 0 (entget blk))) "INSERT")
    (progn
      (setq attEnt (entnext blk))
      (while (and attEnt (= (cdr (assoc 0 (entget attEnt))) "ATTRIB"))
        (progn
          (setq attList (cons (entget attEnt) attList))
          (setq attEnt (entnext attEnt))
        )
      )
      (reverse attList)
    )
  )
)
(princ "\n\tSETCOLOR Command Loaded; written by Tiger")
(princ)
