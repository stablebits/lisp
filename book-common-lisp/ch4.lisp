
;;; 1. Rotate an (n, n) matrix 90 degrees clockwise.

(defun gen-base-swap-path (n base)
  (let ((n-th (- n 1)))
    ;; mapcar just to avoid repeatative typing.
    (mapcar #'(lambda (x)
                (cons (+ (first x) (first base))
                      (+ (rest x) (rest base))))
            (list (cons 0 0) (cons 0 n-th) (cons n-th 0) (cons n-th n-th)))))

 
