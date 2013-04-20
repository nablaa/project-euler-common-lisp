(defun solve ()
  (routes 20 20))

(defun routes (n m)
  (combination (+ n m) n))

(defun combination (n k)
  (/ (fact n) (* (fact k) (fact (- n k)))))

(defun fact (n)
  (if (= n 0)
    1
    (* n (fact (- n 1)))))
