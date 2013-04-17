(defun solve ()
  (product-of-triplet))

(defun product-of-triplet ()
  (reduce #'* (find-triplet)))

(defun find-triplet ()
  (loop :for a :from 1 :to 1000 do
        (loop :for b :from (+ a 1) :to 1000 do
              (let ((c (- 1000 a b)))
                (when (and (> c b a) (is-pythagorean-triplet a b c))
                  (return-from find-triplet (list a b c)))))))

(defun is-pythagorean-triplet (a b c)
  (= (+ (* a a) (* b b)) (* c c)))
