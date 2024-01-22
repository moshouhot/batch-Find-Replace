(defun C:ssgetdetail (/ SS N E DATA)
  ; 选择实体
  (setq SS (ssget))
  (setq N (1- (sslength SS)))
	
  ; 遍历实体并提取数据
  (repeat (1+ N)
    (setq E (entget (ssname SS N)))
    (setq DATA (cons E DATA))
    (setq N (1- N))
  )
	
  ; 打印实体数据
  (foreach E DATA
    (princ "\n")
    (princ E)
    (princ "\n")
  )
	
  ; 打印分隔符
  (princ "\n*********************************\n")
	
  ; 解释实体数据
  (foreach E DATA
    (princ "\n实体类型：")
    (princ (cdr (assoc 0 E)))
    (princ "\n")
    (princ "图元的 handle（唯一标识符）：")
    (princ (cdr (assoc -1 E)))
    (princ "\n")
    (princ "实体的 object ID：")
    (princ (cdr (assoc 5 E)))
    (princ "\n")
    (princ "字体样式：")
    (princ (cdr (assoc 7 E)))
    (princ "\n")
    (princ "实体位于哪个布局空间410：")
    (princ (cdr (assoc 410 E)))
    (princ "\n")
    (princ "实体所属的图层名称8：")
    (princ (cdr (assoc 8 E)))
    (princ "\n")
    (princ "实体颜色65：")
    (princ (cond
            ((assoc 62 E) (cdr (assoc 62 E)))
             (T "bylayer")
           ))
    (princ "\n")
    (princ "实体起点坐标10：")
    (princ (cdr (assoc 10 E)))
    (princ "\n")
    (princ "实体终点坐标11：")
    (princ (cdr (assoc 11 E)))
    (princ "\n")
    (princ "实体法向量：")
    (princ (cdr (assoc 210 E)))
    (princ "\n")
  )
  (princ)
)
