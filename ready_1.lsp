;48. ������� GET ���������� � �������� ���������� NIL   
;� ��� ������, ���� � ������� ��� ������� ��������, ����
;���� ��������� ����� �������� �������� NIL.
;�������������, �������� GET ������ ���������, ���� �� ��������� �������� �
;������ �������. �������� �������� (�����-�������� ������ ��������),
;������� ���������, �������� �� ������ ������ ���������.

(defun prov (symb sv)
	(prov_sv sv (symbol-plist symb))
)



(defun prov_sv(sv list)
  (cond
      ((null list) nil)
      ((equal sv (car list)) T)
      (t (prov_sv sv (cddr list)))
  )
)
(write-line "������ 48")
;;; Test 1
(setf (get 'cars 'color) 'nil)
(setf (get 'cars '���) '�����)


;;; Test 1
(print "Test 1")
(print (symbol-plist 'cars))
(print "(prov cars color)")
(print (prov 'cars 'color))
(write-line "")

;;; Test 2
(print "Test 2")
(print (symbol-plist 'cars))
(print "(prov cars col)")
(print (prov 'cars 'col))
(write-line "")
