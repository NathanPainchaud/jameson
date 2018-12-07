;; Function that displays the statistics about the performance of the model
(defun print-analysis (grouped-results)
  (format t "~%")
  (dolist (group grouped-results)
    (format t "   (From trial ~d to trial ~d)    ~C Hits: ~d    ~C Mean time: ~3$ ~%"
              (result-group-start-trial group)
              (result-group-end-trial group)
              #\tab
              (result-group-hits group)
              #\tab
              (result-group-mean-time group))))

; Function that summarizes the results according to groups of trial
(defun group-results (results results-group-size)
  (let ((grouped-results nil)
        (hits-in-trial-group 0)
        (time-for-trial-group 0))
    (dotimes (result-idx (length results) (reverse grouped-results))
      (let ((result (pop results)))

        (when (result-hit result)
          (incf hits-in-trial-group))
        (setf time-for-trial-group (+ time-for-trial-group (result-time result)))

        (when (or (null results)
                  (= 0 (mod (1+ result-idx) results-group-size)))
          (let ((mean-time-for-trial-group (/ time-for-trial-group results-group-size)))
            (push (make-result-group :start-trial (1+ (* results-group-size (floor result-idx results-group-size)))
                                     :end-trial (1+ result-idx)
                                     :hits hits-in-trial-group
                                     :mean-time mean-time-for-trial-group)
                  grouped-results))
          (setf hits-in-trial-group 0)
          (setf time-for-trial-group 0))))))

;; Function that computes the statistics about the performance of the model
(defun analyze-results (results results-group-size)
  (print-analysis (group-results results results-group-size)))