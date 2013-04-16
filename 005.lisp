(defun solve ()
  (smallest-evenly-divisible 20))

(defun smallest-evenly-divisible (max-divisor)
  (let ((increment (prime-factors-multplied max-divisor)))
    (loop :for n :from increment :by increment
          :until (evenly-divisible n max-divisor)
          :finally (return n))))

(defun evenly-divisible (num max-divisor)
  (loop :for d :from max-divisor :above 1
        :do (when (/= (mod num d) 0)
              (return-from evenly-divisible nil)))
  t)

(defun prime-factors-multplied (n)
  (reduce #'* (remove-if #'null (sieve n))))

(defun sieve (n)
  (let ((numbers (make-array (- n 2))))
    (loop for x from 2 to (- n 1) do (setf (aref numbers (- x 2)) x))
    (loop for x from 0 to (- n 3) do (let ((elem (aref numbers x)))
                                       (when (not (null elem))
                                         (setq index (+ x elem))
                                         (loop while (< index (length numbers)) do
                                               (setf (aref numbers index) nil)
                                               (setf index (+ index elem))))))
    numbers))
