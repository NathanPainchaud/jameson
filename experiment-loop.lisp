;;;; File that encapsulates the functions that run the model on each timestep of an experiment and collect the results

(defparameter *run-model* t)

;; Function that sets up the window and displays the layout of a timestep
(defun present-timestep (timestep &optional (new-window t))
  (let ((window (if new-window
                    (open-exp-window "Jameson Experiment" :visible (timestep-visible timestep))
                    nil)))
    
    (unless new-window
      (clear-exp-window))
    
    (present-projectiles (timestep-projectiles timestep))
    (present-jameson *jameson*)

    (when new-window
      (install-device window))

    (proc-display :clear t)

    (setf *start-time* (get-time *run-model*))

    window))

;; Function that runs the window handler until results for each timestep
(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (let* ((timestep (pop *timesteps*))
         (timestep-time (/ (- (get-time *run-model*) *start-time*) 1000.0)))
    (if (= (timestep-order-in-trial timestep) (1- *timesteps-by-trial*))
      (progn
        (push (make-result :hit (is-jameson-hit *jameson* (timestep-projectiles timestep))
                           :time *trial-time*)
              *results*)
        (setf *trial-time* 0))
      (setf *trial-time* (+ *trial-time* timestep-time)))
    (move-jameson-on-decision *jameson* (string key))
    (when *timesteps*
      (present-timestep (first *timesteps*) nil))))

;; Function that allows the window handler to run until results have been collected for all timesteps
(defun collect-responses (timestep-count)
  (setf *results* nil)
  (setf *trial-time* 0)
  (let ((window (present-timestep (first *timesteps*))))
    (if *run-model*
      (run (* *max-response-time* timestep-count) :real-time (and *visible* *real-time*))
      (while (null *results*)
        (allow-event-manager window)))))
