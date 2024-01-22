;�����������ļ��н�ϵͳ�ļ�����·��
(setq sp (getenv "ACAD"))
(setq path '("D:\\CAD APPLOAD" "D:\\CAD APPLOAD\\�Զ�����" "D:\\CAD APPLOAD\\�Զ�����\\������"))
;(foreach x path (setq sp (strcat sp ";" x))) ;�ļ�����·���������
(foreach x path (setq sp (strcat x ";" sp))) ;�ļ�����·��������ǰ
(setenv "ACAD" sp)
;֪���ļ������س���
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
  (setq fileName "���ٸ�ɫgs(Sammy�޸İ�)v1.2")

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
  (setq fileName "��LO�� ͼֽ����_1.7")

  (if (not (vl-vlx-loaded-p fileName))
    (load-coldstart-file fileName)
  )
  (c:lo)
)


(defun C:JSQ ()
  (setq fileName "��JSQ������CAD������V1.1(�س�����)")

  (if (not (vl-vlx-loaded-p fileName))
    (load-coldstart-file fileName)
  )
  (c:JSQ)
)

(defun C:QX ()
  (setq fileName "��QX��Բ����Ҫ�� ��ǿ�� modify by tiger")

  (if (not (vl-vlx-loaded-p fileName))
    (load-coldstart-file fileName)
  )
  (c:QX)
)