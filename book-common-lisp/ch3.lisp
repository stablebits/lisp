
; Excersises from Chapter 3 of "ANSI Common Lisp".

; 5.
(defun rec-pos+ (lst)
  (reverse (do-rec-pos+ 0 '() (first lst) (rest lst))))

(defun do-rec-pos+ (pos result elm tail)
  (if (null elm)
    result
    (do-rec-pos+ (+ pos 1) (cons (+ pos elm) result) (first tail) (rest tail))))

(defun iter-pos+ (lst)
  (let ((pos 0)
        (result '()))
    (dolist (elm lst)
      (setf result (cons (+ pos elm) result))
      (setf pos (+ pos 1)))
    (reverse result)))

(defun mapcar-pos+ (lst)
  (let ((pos 0))
    (mapcar #'(lambda (x)
                (let ((ret (+ pos x)))
                  (setf pos (+ pos 1))
                  ret))
            lst)))

;
; Sequence generator => (0, 1, 2, ...)
; 
; (setf cnt (counter))
; (funcall cnt) => 0
; (funcall cnt) => 1
; ...
;
(defun counter ()
  (let ((pos 0))
    (lambda ()
      (let ((curr pos))
        (setf pos (+ pos 1))
        curr))))

; 3.
(defun occurences (lst)
  (sort (count-occurences '() (first lst) (rest lst))
        #'> :key #'rest))
;        #'(lambda (x y) (> (rest x) (rest y)))))

(defun count-occurences (pairs elm lst)
  (if (null elm)
    pairs
    (count-occurences
      (update-pairs pairs elm lst)
      (first lst)
      (rest lst))))

(defun update-pairs (pairs elm lst)
  (if (assoc elm pairs)
    pairs
    (let ((n 1))
      (dolist (elm2 lst)
        (and (eql elm elm2) 
             (setf n (+ n 1))))
      (cons (cons elm n) pairs))))
  
; 2.
(defun new-union (lst1 lst2)
  (reverse (add-one-of-each '() (first lst1) (first lst2) (rest lst1) (rest lst2))))

(defun add-one-of-each (result elm1 elm2 tail1 tail2)
  (if (and (null elm1) (null elm2))
    result
    (add-one-of-each 
      (let* ((res1 (if (not (null elm1))
                    (adjoin elm1 result)
                    result))
            (res2 (if (not (null elm2))
                    (adjoin elm2 res1)
                    res1)))
        res2)
      (first tail1) (first tail2) (rest tail1) (rest tail2))))
