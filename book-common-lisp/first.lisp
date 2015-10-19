
(defun my-member-if (fn lst)
  (and (consp lst)
    (if (funcall fn (first lst))
      lst
      (my-member-if fn (rest lst)))))

(defun power (num pow)
  (if (= pow 0)
    1
    (* num (power num (- pow 1 )))
  )
)

(defun count-atoms (arg)
  (cond ((null arg) 0)
        ((atom arg) 1)
        (t (+ (count-atoms (first arg)) (count-atoms (rest arg)) ))
        )
  )

(defun count-anywhere (x arg)
  (cond ((null arg) 0)
        ((atom arg) (if (eql x arg) 1 0))
        (t (+ (count-anywhere
                x
                (first arg)
                )
              (count-anywhere
                x
                (rest arg)
                ))))
  )

(defun count-anywhere2 (item tree)
  (cond ((equal item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere2 item (first tree))
              (count-anywhere2 item (rest tree))))))
