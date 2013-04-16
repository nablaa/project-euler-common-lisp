(defun solve ()
  (smallest-evenly-divisible 20))

(defun smallest-evenly-divisible (n)
  (reduce #'lcm (loop :for i :from 1 :to n :collect i)))
