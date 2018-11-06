;;;; File that defines the functions of the program callable by the user

;; For the files to load correctly, the Lisp environment's load path must be set to the directory of the project
;; The load order of the dependencies MUST NOT BE ALTERED for the program to execute properly
(load (merge-pathnames "config.lisp" *load-truename*))
(load (merge-pathnames "global.lisp" *load-truename*))

(load (merge-pathnames "struct.lisp" *load-truename*))
(load (merge-pathnames "projectile.lisp" *load-truename*))
(load (merge-pathnames "jameson.lisp" *load-truename*))

(load (merge-pathnames "experiment-setup.lisp" *load-truename*))
(load (merge-pathnames "experiment-loop.lisp" *load-truename*))
(load (merge-pathnames "results.lisp" *load-truename*))

;; Definition of parameters that MUST NOT BE ALTERED directly by the user,
;; because they would require modifications to be made to the code
(defparameter *timesteps-by-trial* 5) ; Parameter that indicates how many times the projectile move in each trial before reaching Jameson's movement axis

;; Function callable by a user of the model that runs a single trial
;; The model is not reset between trials so that it can learn
;; A trial is defined as an experiment that lasts from the generation of the projectiles
;; to the projectiles intersecting with Jameson's movement axis
(defun jameson-trial (&optional (visible *visible*))
  (setf *jameson* (make-instance 'jameson))
  (setf *timesteps* (create-trial visible))
  (collect-responses *timesteps-by-trial*)
  (analyze-results))

;; Function callable by a user of the model that runs a number of trials
;; The model is not reset between trials so that it can learn
(defun jameson (n &optional (visible *visible*))
  (setf *jameson* (make-instance 'jameson))
  (setf *timesteps* nil)
  (dotimes (i n)
    (setf *timesteps* (append *timesteps* (create-trial visible))))
  (collect-responses (* *timesteps-by-trial* n))
  (analyze-results))
