;***************************************************************************
;;;����ͼ���Կ��ɫ����������Ժ��ԺTiger��gpt����
;;;***************************************************************************
(defun set-attribute-color (blkEntity color / attList attEnt attEntData)
  ; ��ȡ���Կ����������
  (setq attList (get-attribute-all-names-and-values blkEntity))
  ; ������������
  (foreach attPair attList
    (setq attEnt (cdr (assoc -1 attPair)))
    ; ������Բ�����62���루��ɫ�����������ɫ�����������ɫ
    (setq attEntData (entget attEnt))
    (if (assoc 62 attEntData)
      (setq attEntData (subst (cons 62 color) (assoc 62 attEntData) attEntData))
      (setq attEntData (append attEntData (list (cons 62 color))))
    )
    (entmod attEntData)
  )
)

(defun c:setcolor ()
  (princ "\nѡ��Ҫ�޸���ɫ�����Կ飨Ĭ��ֻѡGC200��:")
  ; ѡ��ͼ���ڵ�����ʵ��
  (setq ss (ssget '((0 . "*"))))
  (setq color (getint "\n��������ɫֵ (Ĭ��8): "))
  (if (= color nil) (setq color 8)) ; ����û�δ������ɫ����ʹ��Ĭ����ɫ1
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
  (princ (strcat "\n�ܹ�ѡ���� " (itoa total-count) " �����Կ飬���� GC200 ���� " (itoa gc200-count) " ����"))
  (princ)
)

; ��ȡ���Կ����������
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
