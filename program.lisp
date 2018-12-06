;;; Definition of parameters that SHOULD NOT BE ALTERED directly by the user, because they would require modifications to be made to the code

; Parameter that indicates how many timesteps the projectile can move in each trial before reaching Jameson's movement axis
(defparameter *timesteps-by-trial* 5)

; Parameter that stores the maximum average that the model can take to respond to the projectiles, otherwise its run is terminated
; This parameter should only be modified if the model is no longer able to take a decision to move at each timestep
; (Such a situation could possibly occur if many projectiles are present and the model as to account for all of them)
(defparameter *max-response-time* 10)

;;; Definition of global variables that are used by multiple functions in the program
(defvar *timesteps*)  ; Variable that stores the information about the display for every timestep in the experiment
(defvar *results*)    ; Variable that stores the results from the experiments
(defvar *jameson*)    ; Variable that stores the object representing Jameson

;;; Definition of variables used to parameterize the ACT-R model
(defvar *learning* t)   ; Variables that indicates if the model should learn from trials

;;; Definition of custom data structures used by the program
(defstruct timestep order-in-trial projectiles) ; Custom data structure to hold the information about the displa of a specific timestep in the experiment
(defstruct result hit time)                     ; Custom data structure to hold the results compute for each trial (on the last timestep of each trial)
(defstruct result-group start-trial end-trial hits time)  ; Custom data structure to hold the results of a group of trials (useful when displaying the results)

;;; Load files for the program
;; For the files to load correctly, the Lisp environment's load path must be set to the directory of the project
;; The load order of the dependencies MUST NOT BE ALTERED for the program to execute properly
(load (merge-pathnames "projectile.lisp" *load-truename*))
(load (merge-pathnames "jameson.lisp" *load-truename*))

(load (merge-pathnames "experiment-setup.lisp" *load-truename*))
(load (merge-pathnames "experiment-loop.lisp" *load-truename*))
(load (merge-pathnames "results.lisp" *load-truename*))

;; Function callable by a user of the model that runs a single trial
;; The model is not reset between trials so that it can learn
;; A trial is defined as an experiment that lasts from the generation of the projectiles to the projectiles intersecting with Jameson's movement axis
(defun jameson-trial (&key (visible t) (projectiles-nb 1))
  (setf *jameson* (make-instance 'jameson))
  (setf *timesteps* (create-trial projectiles-nb *timesteps-by-trial*))
  (collect-responses *timesteps-by-trial* visible))

;; Function callable by a user of the model that runs a number of trials
;; The model is not reset between trials so that it can learn
;; A trial is defined as an experiment that lasts from the generation of the projectiles to the projectiles intersecting with Jameson's movement axis
(defun jameson (n &key (visible nil) (projectiles-nb 1) (results-group-size 10))
  (setf *jameson* (make-instance 'jameson))
  (setf *timesteps* nil)
  (dotimes (i n)
    (setf *timesteps* (append *timesteps* (create-trial projectiles-nb *timesteps-by-trial*))))
  (collect-responses (* *timesteps-by-trial* n) visible)
  (analyze-results results-group-size))
