(defun solve ()
  (solve-n 2000000))

(defun next-fibonacci (l)
  (+ (nth (- (length l) 1) l)
     (nth (- (length l) 2) l)))

(defun fibonaccies-smaller-than (n generated)
  (if (> (next-fibonacci generated) n)
    generated
    (fibonaccies-smaller-than n (append generated (list (next-fibonacci generated))))))

(defun generate-fibonaccies-smaller-than (n)
  (fibonaccies-smaller-than n '(1 1)))

(defun solve-n (n)
  (apply #'+ (remove-if-not #'evenp (generate-fibonaccies-smaller-than n))))
