;; Generates the different timesteps of a trial, from when the projectile was generated
;; to when it intersects with Jameson's axis
(defun create-trial (projectiles-nb timesteps-by-trial)
  (let ((timesteps (list (make-timestep :order-in-trial 0
                                        :projectiles (generate-projectiles projectiles-nb)))))
    (dotimes (i (1- timesteps-by-trial) timesteps)
      (push (make-timestep :order-in-trial (1+ i)
                           :projectiles (mapcar #'move-projectile (timestep-projectiles (first (last timesteps)))))
            (cdr (last timesteps))))))
