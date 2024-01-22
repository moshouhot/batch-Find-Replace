;***************************************************************************
;;;���Կ�����ˢ����������Ժ��ԺTiger��gpt����
;;;***************************************************************************

;; ���� bgg �����
(defun c:bgg ()
  (vl-load-com) ; ���� Visual LISP ������
  ;; ѡ��Դ���Կ�
  (setq sourceBlk (entsel "\nѡ��Դ���Կ飺"))
  (if sourceBlk
    (progn
      ;; ��ȡ���еĲ���ͼ��
      (setq ss1 (ssget '((0 . "INSERT"))))
      (setq ss1_sslen (sslength ss1))
      (setq ssn 0)
      ;; �������еĲ���ͼ��
      (while (< ssn ss1_sslen)
        (setq en1 (ssname ss1 ssn))
        ;; ��鵱ǰ������ͼ���Ƿ���Դ���Կ���ͬ
        (if (= (cdr (assoc 2 (entget en1))) (cdr (assoc 2 (entget (car sourceBlk)))))
          ;; ������������
          (copy-attribute-settings (car sourceBlk) en1)
        )
        (setq ssn (+ 1 ssn))
      ))
  )
  (princ) ; �˳���������ʾ���
)

;; ���µ�������ֵ�ĺ���
(defun OAPUT_ONEATT_ZBV (EN ATTNAME int VALUE / RETURN E TEST ENT)
  ;; ��ʼ������
  (setq E EN
        RETURN NIL
        TEST t
  )
  ;; ����ʵ�������
  (while (and TEST (setq E (entnext E)))
    (setq ENT (entget E))
    (cond
      ;; ���ʵ�岻�� ATTRIB������ֹѭ��
      ((not (= (cdr (assoc 0 ENT)) "ATTRIB"))
       (setq TEST NIL)
      )
      ;; ���ʵ���� SEQEND������ֹѭ��
      ((= "SEQEND" (cdr (assoc 0 ENT)))
       (setq TEST NIL)
      )
      ;; ����ҵ���Ҫ���µ�����
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
      ) ; ����cond
    )
  ;; ���ؽ��
  RETURN
)

;; ��ȡ���Կ��е��������ƺ�����ֵ�ĺ���
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

;; �����������õĺ���
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

