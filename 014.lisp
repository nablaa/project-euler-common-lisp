(defun solve ()
  (longest-chain-under-n 1000000))

(defun longest-chain-under-n (n)
  (let ((max-value 0)
        (index 1))
    (loop :for i :from 1 :below n :if (> (chain-length i) max-value)
          :do (setf max-value (chain-length i)) (setf index i))
    index))

(let ((hash (make-hash-table)))
  (defun chain-length (n)
    (if (= n 1)
      1
      (if (null (gethash n hash))
        (setf (gethash n hash) (+ 1 (chain-length (collatz n))))
        (gethash n hash)))))

(defun collatz (n)
 (if (evenp n)
   (/ n 2)
   (+ (* 3 n) 1)))
