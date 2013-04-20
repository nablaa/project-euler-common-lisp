(defun solve ()
  (reduce #'+ (amicable-numbers-under-n 10000)))

(defun amicable-numbers-under-n (n)
  (let ((amicable-numbers nil))
    (loop :for i :from 1 :below n :do
          (let ((sum (sum-of-proper-divisors i)))
            (when (and (/= sum i) (= (sum-of-proper-divisors sum) i))
              (push i amicable-numbers))))
    amicable-numbers))

(defun amicable-numbers-under-n-temp (n)
  (loop :for i :from 1 :below n
        :if (and (/= (sum-of-proper-divisors i) i) (= (sum-of-proper-divisors (sum-of-proper-divisors i)) i)) :collect (list i (sum-of-proper-divisors i))))

(defun sum-of-proper-divisors (n)
  (reduce #'+ (proper-divisors n)))

(defun proper-divisors (n)
  (loop :for d :from 1 :below n if (= (mod n d) 0) :collect d))
