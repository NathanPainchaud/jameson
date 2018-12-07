(defvar *start-time*) ; Variable used to calculate the time taken by the model to take each decision
(defvar *trial-time*) ; Variable that stores the total time taken by the model to take decisions during the trial
(defvar *timesteps*)  ; Variable that stores the information about the display for every timestep in the trial
(defvar *result*) ; Variable that stores the result for the trial

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

    (unless (= (timestep-order-in-trial timestep) (1- *timesteps-by-trial*))
      (proc-display :clear t))

    (setf *start-time* (get-time *run-model*))))

;; Function that calls the model with the result of a trial to allow the model to learn from it
(defun show-model-trial-result (results)
  (if (result-hit results)
    (mod-focus-fct `(state ,'results result ,"hit"))
    (mod-focus-fct `(state ,'results result ,"not-hit")))
  (run *max-response-time*))

;; Function that updates the environment, the statistics and the model himself according
;; to the response of the model to a timestep
(defun handle-timestep-response (timestep action)
  (let ((timestep-time (/ (- (get-time *run-model*) *start-time*) 1000.0)))
    (if (cdr *timesteps*)
      (progn
        (setf *trial-time* (+ *trial-time* timestep-time))
        (move-jameson-on-decision *jameson* action))
      (setf *result* (make-result :hit (is-jameson-hit *jameson*
                                                       (timestep-projectiles timestep))
                                  :time *trial-time*)))))

;; Function that runs the window handler until results for each timestep
(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (handle-timestep-response (first *timesteps*) (string key)))

;; Function that runs the model for each timestep of a trial
;; and provides the result as feedback to the model
(defun run-trial (trial-timesteps visible)
  (setf *result* nil)
  (setf *trial-time* 0)
  (setf *timesteps* trial-timesteps)
  (setf *jameson* (make-instance 'jameson))
  (present-timestep (first *timesteps*) :new-window t :visible visible)
  (dotimes (i (1- *timesteps-by-trial*))
    (pop *timesteps*)
    (run *max-response-time* :real-time visible)
    (present-timestep (first *timesteps*)))
  (show-model-trial-result *result*)
  *result*)