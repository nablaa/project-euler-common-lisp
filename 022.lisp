(load #p"names.lisp")

(defun solve ()
  (total-score input))

(defun total-score (names)
  (reduce #'+ (names-to-scores names)))

(defun names-to-scores (names)
  (let ((alphabetical-scores (names-to-alphabetical-scores names)))
    (loop :for i :from 0 :below (length alphabetical-scores)
          :collect (* (nth i alphabetical-scores) (+ i 1)))))

(defun names-to-alphabetical-scores (names)
  (let ((sorted (sorted-names names)))
    (mapcar #'alphabetical-score sorted)))

(defun sorted-names (names)
  (sort names #'string<))

(defun alphabetical-score (text)
  (reduce #'+ (map 'list #'(lambda (x) (- (char-int x) (char-int #\A) -1)) text)))
