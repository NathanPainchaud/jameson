(defvar *start-time*) ; Variable used to calculate the time taken by the model to take each decision
(defvar *trial-time*) ; Variable that stores the total time taken by the model to take decisions during the trial

(defparameter *run-model* t)

;; Function that sets up the window and displays the layout of a timestep
(defun present-timestep (timestep &key (new-window nil) (visible t))
  (let ((window (if new-window
                    (open-exp-window "Jameson Experiment" :visible visible)
                    nil)))
    
    (unless new-window
      (clear-exp-window))
    
    (present-projectiles (timestep-projectiles timestep))
    (present-jameson *jameson*)

    (when new-window
      (install-device window))

    (proc-display :clear t)

    (setf *start-time* (get-time *run-model*))))

;; Function that calls the model with the results of the trial to allow the model to learn from them
(defun show-model-results (results)
  (if (result-hit results)
    (mod-focus-fct `(state ,'results result ,"hit"))
    (mod-focus-fct `(state ,'results result ,"not-hit"))))

;; Function that updates the environment, the statistics and the model himself according
;; to the response of the model to a timestep
(defun handle-response (timestep action)
  (let ((timestep-time (/ (- (get-time *run-model*) *start-time*) 1000.0)))
    (if (= (timestep-order-in-trial timestep) (1- *timesteps-by-trial*))
      (progn
        (setf *results*
              (append *results*
                      (list (make-result :hit (is-jameson-hit *jameson* (timestep-projectiles timestep))
                                         :time *trial-time*))))
        (setf *trial-time* 0)
        (show-model-results (first (last *results*)))
        (run *max-response-time*))
      (progn
        (setf *trial-time* (+ *trial-time* timestep-time))
        (move-jameson-on-decision *jameson* action)))))

;; Function that runs the window handler until results for each timestep
(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (handle-response (first *timesteps*) (string key)))

;; Function that runs the model for each timestep to collect the responses of the model
(defun collect-responses (timestep-count visible)
  (setf *results* nil)
  (setf *trial-time* 0)
  (setf *jameson* (make-instance 'jameson))
  (present-timestep (first *timesteps*) :new-window t :visible visible)
  (dotimes (i timestep-count)
    (pop *timesteps*)
    (when *timesteps*
      (run *max-response-time* :real-time visible)
      (setf *jameson* (make-instance 'jameson))
      (present-timestep (first *timesteps*)))))