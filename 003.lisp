(defun solve ()
  (largest-prime-factor 600851475143))

(defun largest-prime-factor (n)
  (apply #'max (factors n)))

(defun factors (n)
  (factor n 2))

(defun factor (n k)
  (cond ((> k (floor (sqrt n))) (list n))
        ((= (mod n k) 0) (cons k (factor (truncate n k) 2)))
        ((= k 2) (factor n 3))
        (t (factor n (+ k 2)))))

