(defun solve ()
  (smallest-evenly-divisible 20))

(defun smallest-evenly-divisible (max-divisor)
  (loop :for n :from max-divisor :by max-divisor
        :until (evenly-divisible n max-divisor)
        :finally (return n)))

(defun evenly-divisible (num max-divisor)
  (loop :for d :from max-divisor :above 1
        :do (when (/= (mod num d) 0)
              (return-from evenly-divisible nil)))
  t)
