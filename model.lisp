;;; Load files for the program
;; For the files to load correctly, the Lisp environment's load path must be set to the directory of the project
(load (merge-pathnames "program.lisp" *load-truename*))

(clear-all)

(define-model jameson

(sgp :v t :esc nil :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil :show-focus t :trace-detail low)

  
;; PRODUCTIONS - let's try to keep them organized
;; GENERAL

(chunk-type position pos-x pos-y)
(chunk-type trajectory position1 position2 position3 course state)
(chunk-type projectile id trajectory)
(chunk-type goal state dummy)

(add-dm
  (goal isa trajectory)(attended)(attending)(done))


(P respond
   =goal>
      ISA         trajectory
      state       done
   ?manual>
     state        free
==>
   +goal>
   +manual>
      cmd         press-key
      key         "s"
)

;; VISUAL-MODULE PRODUCTION

(P attend-projectile
   =goal>
      ISA         trajectory
      state       nil
   =visual-location>
   		screen-x				=pos-x
   		screen-y				=pos-y
   ?visual>
      state       free
==>
   +visual>
      cmd         move-attention
      screen-pos  =visual-location
   =goal>
      state       attended
)

(P encode-projectile
   =goal>
      ISA         trajectory
      state       attended
   =visual>
      value       =letter
==>
   =goal>
      state       done
)


(set-all-base-levels 100000 -1000)
(goal-focus goal)
)
