(defun solve ()
  (sum-of-digits (power-of-2 1000)))

(defun power-of-2 (n)
  (ash 2 (- n 1)))

(defun sum-of-digits (n)
  (reduce #'+ (number-to-digits n)))

(defun number-to-digits (n)
  (if (< n 10)
    (list n)
    (cons (mod n 10) (number-to-digits (truncate n 10)))))

