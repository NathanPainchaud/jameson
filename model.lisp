;;; Load files for the program
;; For the files to load correctly, the Lisp environment's load path must be set to the directory of the project
(load (merge-pathnames "program.lisp" *load-truename*))

(clear-all)

(define-model jameson

(sgp :v nil :esc t :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil)

(sgp :show-focus t)

(chunk-type position position-x position-y)
(chunk-type trajectory position1 position2 position3 course)
(chunk-type projectile id trajectory)
(chunk-type goal state)

(add-dm
  (goal isa goal)
  (attending))

(P attend
   =goal>
      ISA         goal
      state       nil
   =visual-location>
   ?visual>
       state      free
==>
   =goal>
      state       attending
)

(P respond
   =goal>
      ISA         goal
      state       attending
   ?manual>
     state        free
==>
   +goal>
   +manual>
      cmd         press-key
      key         "s"
)

(set-all-base-levels 100000 -1000)
(goal-focus goal)
)
