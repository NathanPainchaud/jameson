;;; Load files for the program
;; For the files to load correctly, the Lisp environment's load path must be set to the directory of the project
(load (merge-pathnames "program.lisp" *load-truename*))

(clear-all)

(define-model jameson

(sgp :v t :esc nil :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil :show-focus t :trace-detail low)

  
;; PRODUCTIONS - let's try to keep them organized
;; GENERAL

(chunk-type position position-x position-y)
(chunk-type trajectory position1 position2 position3 course)
(chunk-type projectile id trajectory)
(chunk-type estimation dx dy y action)
(chunk-type goal state tracked pos-x pos-y)

(add-dm
  (goal isa goal)
  (attending)(attended)(retrieving)(done))


(P respond
   =goal>
      ISA         goal
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
      ISA         goal
      state       nil
   =visual-location>
         screen-x          =pos-x
         screen-y          =pos-y
   ?visual>
      state       free
==>
   +visual>
      cmd         move-attention
      screen-pos  =visual-location
   =goal>
      state       attended
      pos-x          =pos-x
      pos-y          =pos-y

)

(P encode-projectile
   =goal>
      ISA         goal
      state       attended
   =visual>
      value       =letter
==>
   =goal>
      state       retrieving
      tracked        =letter
)

;; Information retireval productions
;; Not tested

(P remmeber-situation
    =goal>
     ISA    goal
     state     retrieving
    =retireval>
     ISA    estimation
     action =act
    ?manual>
     state free
   ==>
    =goal>
      state nil
    +manual>
     cmd press-key
     key =act
     
    @retrieval>)

(P cant-remember-up
    =goal>
      ISA goal
      state retrieving
    = y (+ pos-y dy)
    <= dy 0
    ?retrieval>
      buffer failure
    ?manual>
      state free
   ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key "u")

(P cant-remember-down
    =goal>
      ISA goal
      state retrieving
    = y (+ pos-y dy)
    > dy 0
    ?retrieval>
      buffer failure
    ?manual>
      state free
   ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key "d")

(P cant-remember-stay
    =goal>
      ISA goal
      state retrieving
      - y  (+ pos-y dy)
    ?retrieval>
      buffer failure
    ?manual>
      state free
   ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key "s")    

(set-all-base-levels 100000 -1000)
(goal-focus goal)
)