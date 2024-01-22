(defun c:get-attr-names-values ()
  (setq selectedBlk (entsel "\nѡ��һ�����Կ�:"))
  (if selectedBlk
    (progn
      (setq blkEntity (car selectedBlk))
      (setq allAttributeValues (get-attribute-all-names-and-values blkEntity))
      (if allAttributeValues
        (progn
          (princ "\n���Կ���������Ƽ���������ֵ:")
          (foreach attrPair allAttributeValues
            (princ (strcat "\n" (cdr (assoc 2 attrPair)) "\n"))
            (foreach item attrPair
              (if (/= (car item) -1)  ; Ignore the entity name
                (princ (strcat "<" (itoa (car item)) ", " (vl-princ-to-string (cdr item)) "> "))
              )
            )
            (princ "\n****************************************************************\n")
          )
        )
        (princ "\nδ�ҵ����ԡ�")
      )
      (setq attributeNamesValues (get-attribute-names-and-several-values blkEntity))
      (if attributeNamesValues
        (progn
          (foreach attrPair attributeNamesValues
            (foreach item (cdr attrPair)
              (if item
                (princ (strcat "<"
                               (cond
                                 ((= (car item) 1) "1��������")
                                 ((= (car item) 2) "2����")
                                 ((= (car item) 6) "6����")
                                 ((= (car item) 7) "7����")
                                 ((= (car item) 8) "8����")
                                 ((= (car item) 10) "10�����")
                                 ((= (car item) 40) "40�ָߡ��뾶")
                                 ((= (car item) 62) "62��ɫ")
                                 )
                               ", " (vl-princ-to-string (cdr item)) "> "
                               )
                  )
                )
            )
          )
        )
        (princ "\nδ�ҵ����ԡ�")
      )
    )
  )
  (princ)
)
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

(defun get-attribute-names-and-several-values (blkEntity)
  (setq allAttributes (get-attribute-all-names-and-values blkEntity))
  (setq filteredAttributes nil)
  (foreach attr allAttributes
    (setq attrData (list (assoc 1 attr) (assoc 2 attr) (assoc 10 attr) (assoc 40 attr) (assoc 62 attr)))
    (setq filteredAttributes (cons attrData filteredAttributes))
  )
  filteredAttributes
)
