(defun solve ()
  (first-with-over-n-divisors 500))

(defun first-with-over-n-divisors (n)
  (loop :for x :from 1 :until (> (divisor-count (triangle-number x)) n)
        :finally (return (triangle-number x))))

(defun triangle-number (n)
  (/ (+ (* n n) n) 2))

(defun divisors (n)
  (loop :for d :from 1 :to n if (= (mod n d) 0) :collect d))

(defun divisor-count (n)
  (selections (groups-to-counts (group-list (factors n)))))

(defun factors (n)
  (factor n 2))

(defun factor (n k)
  (cond ((> (* k k) n) (list n))
        ((= (mod n k) 0) (cons k (factor (truncate n k) 2)))
        ((= k 2) (factor n 3))
        (t (factor n (+ k 2)))))

(defun group-list (elems)
  (group-list-acc elems nil))

(defun groups-to-counts (elems)
  (mapcar #'length elems))

(defun group-list-acc (elems acc)
  (if (null elems)
    (list acc)
    (if (or (null acc) (= (car acc) (car elems)))
      (group-list-acc (cdr elems) (cons (car elems) acc))
      (append (list acc) (group-list-acc elems nil)))))

(defun selections (ns)
  (reduce #'* (mapcar #'(lambda (x) (+ x 1)) ns)))
