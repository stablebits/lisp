
(defun compress (lst)
  (make-compressed-list '() (first lst) 1 (rest lst)))

(defun decompress (lst)
  (make-decompressed-list '() (first lst) (rest lst)))

(defun make-compressed-list (out elm n in)
  (if (null elm)
    out
    (let ((next (first in))
         (rest-in (rest in)))
      (if (or (null next) (not (= elm next)))
        (make-compressed-list (cons (encode-element elm n) out) next 1 rest-in)
        (make-compressed-list out elm (+ n 1) rest-in)))))
          
(defun make-decompressed-list (out elm tail)
  (if (null elm)
    out
    (let ((new-out
            (if (consp elm)
              (unroll-pair out elm)
              (cons elm out))))
      (make-decompressed-list new-out (first tail) (rest tail)))))

(defun encode-element (elm n)
  (if (= n 1)
    elm
    (cons elm n)))

(defun unroll-pair (out pair)
  (let ((elm (first pair))
        (n (rest pair)))
    (if (= n 1)
      (cons elm out)
      (unroll-pair (cons elm out) (cons elm (- n 1))))))
