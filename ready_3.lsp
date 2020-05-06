;1. ���������� ������, ������� ���������� ���� �����.

(defmacro self () ''(self))

(print (self))


;3. ���������� ���������� ����� (IF ������� p q) � ���� �������.

(defmacro iff (cond p q)
    `(if ,cond ,p ,q))

;;; Test 1
(write-line "Test 1")
(princ " >> (= 5 2) 'ravno 'ne_ravno")
(print (iff (= 5 2) 'ravno 'ne_ravno))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (= 5 5) 'ravno 'neravno")
(print (iff (= 5 5) 'ravno 'neravno))
(write-line "")

(write-line "")



; 4. ���������� � ���� ������� ����� (FIF ���� ��� ���� �����).

(defmacro fif (test n z p)
  `(cond
     ((< ,test 0) ,n)
     ((= ,test 0) ,z)
     (t ,p)))


;;; Test 1
(write-line "Test 1")
(princ " >> -4 '- '= '+")
(print (fif -4 '- '= '+))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> 0 '- '= '+")
(print (fif 0 '- '= '+))
(write-line "")

;;; Test 3
(write-line "Test 2")
(princ " >> 10 '- '= '+")
(print (fif 10 '- '= '+))
(write-line "")

(write-line "")