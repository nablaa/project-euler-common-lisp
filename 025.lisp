(defun solve ()
  (first-fibonacci-with-n-digits 1000))

(defun first-fibonacci-with-n-digits (n)
  (let ((divisor (expt 10 (1- n))))
    (loop :for i :from 1 :do
          (when (= (truncate (fib i) divisor) 1)
            (return-from first-fibonacci-with-n-digits i)))))

(let ((fibs (make-hash-table)))
  (defun fib (n)
    (if (<= n 2)
      1
      (if (null (gethash n fibs))
        (setf (gethash n fibs) (+ (fib (- n 1)) (fib (- n 2))))
        (gethash n fibs)))))

(defun number-to-digits (n)
  (if (< n 10)
    (list n)
    (cons (mod n 10) (number-to-digits (truncate n 10)))))
