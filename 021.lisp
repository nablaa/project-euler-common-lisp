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
  (factors-to-proper-divisors (factors n)))

(defun factors (n)
  (factor n 2))

(defun factor (n k)
  (cond ((> (* k k) n) (list n))
        ((= (mod n k) 0) (cons k (factor (truncate n k) 2)))
        ((= k 2) (factor n 3))
        (t (factor n (+ k 2)))))

(defun factors-to-proper-divisors (factors)
  (let ((self (reduce #'* factors)))
    (sort (remove self (remove-duplicates (to-proper-divs-acc factors 1))) #'<)))

(defun to-proper-divs-acc (factors num)
  (if (null factors)
    (list num)
    (let ((factor (car factors))
          (remaining (cdr factors)))
      (append (to-proper-divs-acc remaining num) (to-proper-divs-acc remaining (* num factor))))))
