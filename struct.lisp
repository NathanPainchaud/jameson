;;;; File that defines the custom data structures used by the program

(defstruct timestep projectiles order-in-trial visible) ; Custom data structure to hold the information about the displa of a specific timestep in the experiment
(defstruct result hit time)                             ; Custom data structure to hold the results compute for each trial (on the last timestep of each trial)