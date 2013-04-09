(defun sum-primes-smaller-than (n)
  (apply #'+ (primes-smaller-than n)))

(defun primes-smaller-than (n)
  (loop for x from 2 to (- n 1) if (prime? x) collect x))

(defun prime? (n)
  (is-prime n 2))

(defun is-prime (n k)
  (cond ((> k (floor (sqrt n))) t)
        ((= (mod n k) 0) nil)
        ((= k 2) (is-prime n 3))
        (t (is-prime n (+ k 2)))))
