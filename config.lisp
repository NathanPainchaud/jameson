;;;; File that defines the program's parameters that can be modified by a user

(defparameter *real-time* t)  ; Parameter that indicates whether to run the model in real-time, or in its own simulated time
(defparameter *visible* t)    ; Parameter that indicates whether or not to display windows when running the experiment

(defparameter *projectiles-nb* 2)     ; Parameter that stores the number of projectiles directed at Jameson at each trial
(defparameter *max-response-time* 10) ; Parameter that stores the maximum average that the model can take to respond to the projectiles, otherwise its run is terminated