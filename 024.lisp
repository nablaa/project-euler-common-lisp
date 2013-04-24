(defun solve ()
  (get-nth-permutation #(0 1 2 3 4 5 6 7 8 9) 1000000))

(defun get-nth-permutation (elems n)
  (dotimes (i (1- n) elems)
    (get-next-permutation elems)))

(defun get-next-permutation (elems)
  (let* ((index (first-non-increasing-index elems))
         (following-index (smallest-number-following-index elems index))
         (start (+ index 1))
         (end (1- (car (array-dimensions elems)))))
    (rotatef (aref elems index) (aref elems following-index))
    (sort-array elems start end)
    elems))

(defun sort-array (elems start end)
  (let* ((len (- end (- start 1)))
         (subarray (make-array len :displaced-to elems :displaced-index-offset start)))
    (sort subarray #'<)
    (loop :for i :from 0 :below len :do
          (setf (aref elems (+ start i)) (aref subarray i)))))

(defun smallest-number-following-index (elems index)
  (let ((value (aref elems index))
        (len (car (array-dimensions elems)))
        (smallest-value nil)
        (smallest-index nil))
    (loop :for index :from (+ index 1) :below len :do
          (let ((candidate (aref elems index)))
            (when (and (> candidate value)
                       (or (null smallest-value) (< candidate smallest-value)))
              (setf smallest-value candidate)
              (setf smallest-index index))))
    smallest-index))

(defun first-non-increasing-index (elems)
  (let ((len (car (array-dimensions elems)))
        (previous nil))
    (loop :for index :from (1- len) :downto 0 :do
          (let ((value (aref elems index)))
            (if (and (not (null previous)) (< value previous))
              (return-from first-non-increasing-index index)
              (setf previous value))))))
