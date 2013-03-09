(defun is-multiple-of-x (n x)
  (= (mod n x) 0))

(defun is-multiple-of-numbers (n numbers)
  (or (mapcar #'(lambda (x) (is-multiple-of-x n x)) numbers)))

(defun multiple-of-any-number (n numbers)
  (reduce #'(lambda (x y) (or x y)) (is-multiple-of-numbers n numbers)))

(defun sum-numbers (n)
  (loop for x from 1 below n by 1 if (multiple-of-any-number x '(3 5)) sum x))
