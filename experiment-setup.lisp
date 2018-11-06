;;;; File that encapsulates the functions that setup the data structures defining the experiments

;; Generates the different timesteps of a trial, from when the projectile was generated
;; to when it intersects with Jameson's axis
(defun create-trial (visible)
  (let ((timesteps (list (make-timestep :projectiles (generate-projectiles *projectiles-nb*)
                                        :order-in-trial 0
                                        :visible visible))))
    (dotimes (i (1- *timesteps-by-trial*) timesteps)
      (push (make-timestep :projectiles (mapcar #'move-projectile (timestep-projectiles (first (last timesteps))))
                           :order-in-trial (1+ i)
                           :visible visible)
            (cdr (last timesteps))))))
