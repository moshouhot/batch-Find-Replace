(defun C:ssgetdetail (/ SS N E DATA)
  ; ѡ��ʵ��
  (setq SS (ssget))
  (setq N (1- (sslength SS)))
	
  ; ����ʵ�岢��ȡ����
  (repeat (1+ N)
    (setq E (entget (ssname SS N)))
    (setq DATA (cons E DATA))
    (setq N (1- N))
  )
	
  ; ��ӡʵ������
  (foreach E DATA
    (princ "\n")
    (princ E)
    (princ "\n")
  )
	
  ; ��ӡ�ָ���
  (princ "\n*********************************\n")
	
  ; ����ʵ������
  (foreach E DATA
    (princ "\nʵ�����ͣ�")
    (princ (cdr (assoc 0 E)))
    (princ "\n")
    (princ "ͼԪ�� handle��Ψһ��ʶ������")
    (princ (cdr (assoc -1 E)))
    (princ "\n")
    (princ "ʵ��� object ID��")
    (princ (cdr (assoc 5 E)))
    (princ "\n")
    (princ "������ʽ��")
    (princ (cdr (assoc 7 E)))
    (princ "\n")
    (princ "ʵ��λ���ĸ����ֿռ�410��")
    (princ (cdr (assoc 410 E)))
    (princ "\n")
    (princ "ʵ��������ͼ������8��")
    (princ (cdr (assoc 8 E)))
    (princ "\n")
    (princ "ʵ����ɫ65��")
    (princ (cond
            ((assoc 62 E) (cdr (assoc 62 E)))
             (T "bylayer")
           ))
    (princ "\n")
    (princ "ʵ���������10��")
    (princ (cdr (assoc 10 E)))
    (princ "\n")
    (princ "ʵ���յ�����11��")
    (princ (cdr (assoc 11 E)))
    (princ "\n")
    (princ "ʵ�巨������")
    (princ (cdr (assoc 210 E)))
    (princ "\n")
  )
  (princ)
)
