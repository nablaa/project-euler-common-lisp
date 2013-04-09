; Elegant, but slow

(defun sum-primes-smaller-than (n)
  (loop for x from 2 to (- n 1) when (prime? x) sum x))

(defun prime? (n)
  (is-prime n 2))

(defun is-prime (n k)
  (cond ((> (* k k) n) t)
        ((= (mod n k) 0) nil)
        ((= k 2) (is-prime n 3))
        (t (is-prime n (+ k 2)))))


; Ugly, but fast

(defun sum-sieve (n)
  (reduce #'+ (remove-if #'null (sieve n))))

(defun sieve (n)
  (let ((numbers (make-array (- n 2))))
    (loop for x from 2 to (- n 1) do (setf (aref numbers (- x 2)) x))
    (loop for x from 0 to (- n 3) do (let ((elem (aref numbers x)))
                                       (when (not (null elem))
                                         (setq index (+ x elem))
                                         (loop while (< index (length numbers)) do
                                               (setf (aref numbers index) nil)
                                               (setf index (+ index elem))))))
    numbers))
