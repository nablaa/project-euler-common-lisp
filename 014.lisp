(defun solve ()
  (longest-chain-under-n 1000000 #'chain-length-hash))

(defun longest-chain-under-n (n func)
  (let ((max-value 0)
        (index 1))
    (loop :for i :from 1 :below n :if (> (funcall func i) max-value)
          :do (setf max-value (funcall func i)) (setf index i))
    index))

(defun chain-length (n)
  (chain-length-acc n 0))

(defun chain-length-acc (n acc)
  (if (= n 1)
    (+ acc 1)
    (chain-length-acc (collatz n) (+ acc 1))))

(let ((hash (make-hash-table)))
  (defun chain-length-hash (n)
    (if (= n 1)
      1
      (if (null (gethash n hash))
        (setf (gethash n hash) (+ 1 (chain-length-hash (collatz n))))
        (gethash n hash)))))

(defun collatz (n)
 (if (evenp n)
   (/ n 2)
   (+ (* 3 n) 1)))
