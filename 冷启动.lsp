;加载冷启动文件夹进系统文件搜索路径
(setq sp (getenv "ACAD"))
(setq path '("D:\\CAD APPLOAD" "D:\\CAD APPLOAD\\自动加载" "D:\\CAD APPLOAD\\自动加载\\冷启动"))
;(foreach x path (setq sp (strcat sp ";" x))) ;文件搜索路径放在最后
(foreach x path (setq sp (strcat x ";" sp))) ;文件搜索路径放在最前
(setenv "ACAD" sp)
;知道文件名加载程序
(defun load-coldstart-file (fileName)
  (if (findfile (strcat fileName ".VLX"))
    (load (strcat fileName ".VLX"))
    (if (findfile (strcat fileName ".FAS"))
      (load (strcat fileName ".FAS"))
      (if (findfile (strcat fileName ".LSP"))
        (load (strcat fileName ".LSP"))
      )
    )
  )
)

(defun C:gs ()
  (setq fileName "快速改色gs(Sammy修改版)v1.2")

  (if (not (vl-vlx-loaded-p fileName))
    (load-coldstart-file fileName)
  )
  (c:gs)
)

(defun C:y ()
  (setq fileName "JianRen58Cracked")

  (if (not (vl-vlx-loaded-p fileName))
    (load-coldstart-file fileName)
  )
  (c:y)
)

(defun C:lo ()
  (setq fileName "【LO】 图纸规整_1.7")

  (if (not (vl-vlx-loaded-p fileName))
    (load-coldstart-file fileName)
  )
  (c:lo)
)


(defun C:JSQ ()
  (setq fileName "【JSQ】阿甘CAD计算器V1.1(回车计算)")

  (if (not (vl-vlx-loaded-p fileName))
    (load-coldstart-file fileName)
  )
  (c:JSQ)
)

(defun C:QX ()
  (setq fileName "【QX】圆曲线要素 加强版 modify by tiger")

  (if (not (vl-vlx-loaded-p fileName))
    (load-coldstart-file fileName)
  )
  (c:QX)
)