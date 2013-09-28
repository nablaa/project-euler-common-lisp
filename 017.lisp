(defun solve ()
  (total-letter-count 1000))

(defun total-letter-count (max-n)
  (loop :for n :from 1 :to max-n :sum (letter-count n)))

(defun letter-count (n)
  (length (filter-chars (number-to-string n))))

(defun filter-chars (s)
  (remove #\- (remove #\space s)))

(defun number-to-string (n)
  (format nil "~R" n))
