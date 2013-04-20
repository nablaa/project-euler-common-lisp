(defun solve ()
  (sum-of-digits (fact 100)))

(defun sum-of-digits (n)
  (reduce #'+ (number-to-digits n)))

(defun fact (n)
  (if (= n 0)
    1
    (* n (fact (- n 1)))))

(defun number-to-digits (n)
  (if (< n 10)
    (list n)
    (cons (mod n 10) (number-to-digits (truncate n 10)))))
