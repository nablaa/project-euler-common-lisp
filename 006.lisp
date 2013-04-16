(defun solve ()
  (difference 100))

(defun difference (n)
  (- (square-of-sum n) (sum-of-squares n)))

(defun sum-of-squares (n)
  (reduce #'+ (loop :for i :from 1 :to n :collect (* i i))))

(defun square-of-sum (n)
  (let ((sum (loop :for i :from 1 :to n :sum i)))
    (* sum sum)))


