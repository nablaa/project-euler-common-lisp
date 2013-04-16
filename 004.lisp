(defun solve ()
  (largest-palindrome-with 999 99))

(defun largest-palindrome-with (max-factor min-factor)
  (let ((acc (list)))
    (loop :for i :from max-factor :above min-factor do
          (loop :for j :from max-factor :above min-factor do
                (let ((num (* i j)))
                  (when (is-palindrome num) (push num acc)))))
    (reduce #'max acc)))

(defun is-palindrome (n)
  (let ((digits (number-to-digits n)))
    (equal digits (reverse digits))))

(defun number-to-digits (n)
  (if (< n 10)
    (list n)
    (cons (mod n 10) (number-to-digits (truncate n 10)))))
