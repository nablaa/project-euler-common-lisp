(defun solve ()
  (reduce #'+ (numbers-not-sum-of-abundants 28123)))

(defun numbers-not-sum-of-abundants (n)
  (let* ((numbers (abundant-numbers-below-n n))
         (len (length numbers))
         (abundants (make-array len :initial-contents numbers))
         (collected (make-array n)))
    (loop :for i :from 0 :below n :do (setf (aref collected i) i))
    (loop :for i :from 0 :below len :do
          (loop :for j :from i :below len :do
                (let ((sum (+ (aref abundants i)
                              (aref abundants j))))
                  (when (< sum n)
                    (setf (aref collected sum) 0)))))
    (remove-if #'zerop (coerce collected 'list))))

(defun abundant-numbers-below-n (n)
  (loop :for i :from 1 :below n :if (is-abundant-number i) :collect i))

(defun is-abundant-number (n)
  (> (reduce #'+ (proper-divisors n)) n))

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
