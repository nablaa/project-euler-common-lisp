(defun solve ()
  (nth-prime 10001))

(defun nth-prime (n)
  (let ((index 0))
    (loop :for num :from 2
          :do (when (prime? num)
                (setf index (+ index 1))
                (when (= index n)
                  (return num)))
          :finally (return num))))

(defun prime? (n)
  (is-prime n 2))

(defun is-prime (n k)
  (cond ((> (* k k) n) t)
        ((= (mod n k) 0) nil)
        ((= k 2) (is-prime n 3))
        (t (is-prime n (+ k 2)))))
