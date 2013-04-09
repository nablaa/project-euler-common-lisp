(defun sum-primes-smaller-than (n)
  (loop for x from 2 to (- n 1) when (prime? x) sum x))

(defun prime? (n)
  (is-prime n 2))

(defun is-prime (n k)
  (cond ((> (* k k) n) t)
        ((= (mod n k) 0) nil)
        ((= k 2) (is-prime n 3))
        (t (is-prime n (+ k 2)))))
