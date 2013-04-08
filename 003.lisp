(defun largest-prime-factor (n)
  (apply #'max (factors n)))

(defun factors (n)
  (factor n 2))

(defun factor (n k)
  (cond ((> k (floor (sqrt n))) (list n))
        ((= (mod n k) 0) (cons k (factor (truncate n k) 2)))
        (t (factor n (+ k 1)))))

