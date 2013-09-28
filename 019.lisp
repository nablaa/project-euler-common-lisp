(defun solve ()
  (count-sundays 1901 2000))

(defun count-sundays (year1 year2)
  (let ((sundays 0))
    (loop :for year :from year1 :to year2 :do
          (loop :for month :from 1 :to 12 :do
                (when (= (weekday 1 month year) 0)
                  (incf sundays 1))))
    sundays))

(defun weekday (day month year)
  (mod (+ (days-between-dates 1 1 1900 day month year) 1) 7))

(defun days-between-dates (day1 month1 year1 day2 month2 year2)
  (let ((days (- (days-from-start-of-year day1 month1 year1))))
    (loop :for year :from year1 :to year2 :do
          (if (= year year2)
            (loop :for month :from (if (= year1 year2) month1 1) :to month2 :do
                  (if (= month month2)
                    (incf days day2)
                    (incf days (days-in-month month year))))
            (loop :for month :from 1 :to 12 :do
                  (incf days (days-in-month month year)))))
    days))

(defun days-from-start-of-year (day month year)
  (let ((days 0))
    (loop :for m :from 1 :to month :do
          (if (= m month)
            (incf days day)
            (incf days (days-in-month m year))))
    days))

(defun days-in-month (month year)
  (case month
    (2 (if (is-leap-year year)
         29
         28))
    ((4 6 9 11) 30)
    (otherwise 31)))

(defun is-leap-year (year)
  (if (= (mod year 4) 0)
    (if (/= (mod year 100) 0)
      t
      (if (= (mod year 400) 0)
        t
        nil))
    nil))
