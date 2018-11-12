# Jameson

## Program structure

The program is split across multiple files, with the name and purpose of each file listed here:

- `model.lisp` Defines the ACT-R model governing Jameson's actions.
- `program.lisp` Core of the Lisp program, that defines global parameters, loads files and defines functions callable by a user.
- `projectile.lisp` Class that defines the attributs and operations concerning a single projectile, along with functions that affect multiplt projectiles.
- `jameson.lisp` Class that defines the attributes and operations concerning Jameson.
- `experiment-setup.lisp` Functions that setup the data structures defining the experiments.
- `experiment-loop.lisp` Functions that run the model on each timestep of an experiment and collect the results.
- `results.lisp` Functions that analyze and display results of experiments.

## Loading the ACT-R model

The file that defines the ACT-R model, and the one that needs to be loaded in the ACT-R environment to run experiments, is the `model.lisp` file.

The user need not bother himself with the other files, as `model.lisp` will take care internally of loading the files necessary to its execution. However, for `model.lisp` to be able to load files properly, it is important that the load path of the Lisp used includes the directory of the project; the path used for loading files are relative and search in the current directory.

For example, if you typically start actr with something like (Closure)`ccl -l load-act-r.lisp `, you could just add a `/jameson` folder from that directory and pull the git there. Then Load Model from the GUI or cmd line.

## Running experiments

Once the Lisp is properly configured and the model is loaded, two functions defined in the program are designed to be called by the user.

For the sake of clarity, we will define a trial as _an experiment that lasts from the generation of the projectiles to the projectiles intersecting with Jameson's movement axis_.

Depending on which of the two functions the user calls, the program can run for a single trial or multiptle trials.

### Execute a single trial

To execute a single trial, simply call the function `jameson-trial`.

The `jameson-trial` function can be run without specifying any parameters. However, it allows for two optional named parameters, described below:
- `visible` (optional, default `t`) Indicates to display a window (`t`) or not (`nil`) when running an experiment. This parameter also affects the execution time of the program. When a window is displayed, experiments are run in real-time, while they are run in the simulated time of the ACT-R model otherwise.
- `projectiles-nb` (optional, default `1`) The number of projectiles directed at Jameson at each trial.

When specifying values for the parameters, a call to the `jameson-trial` function with the default values would look like this:
```lisp
(jameson-trial :visible t :projectiles-nb 1)
```

### Execute multiple trials
To execute multiple trials, simply call the function `jameson`.

The `jameson` function takes a minimum of one parameter to run. However, it allows for three more optional named parameters. All parameters are described below:
- `n` (required) The number of trials to run in the experiment.
- `visible` (optional, default `nil`) Indicates to display a window (`t`) or not (`nil`) when running an experiment. This parameter also affects the execution time of the program. When a window is displayed, experiments are run in real-time, while they are run in the simulated time of the ACT-R model otherwise.
- `projectiles-nb` (optional, default `1`) The number of projectiles directed at Jameson at each trial.
- `results-group-size` (optional, default `10`) When displaying the results of multiple trials, statistics are computed over groups of trials rather than individual trials to avoid overcrowding the output stream. This argument dictates the size of the groups over which to compute the statistics (it is suggested to increase the value when running a large numbers of trials).

When specifying values for the parameters, a call to the `jameson` function for 100 trials with the default values would look like this:
```lisp
(jameson 100 :visible nil :projectiles-nb 1 :results-group-size 10)
```

# Model - technical elements

## Parameter values (sgp ...)
:v [nil|t]  - sets the verbose mode
:esc t      - sets subsymbolic computations
:lf 0.4     - latency factor (default 1.0)
:bll 0.5    - base level learning (default 0.5)
:ans 0.5    - activation noise (.e.g instantaneous noise)
:rt 0.0     - retrieval treshold (chunks) (default 0.0
:ncnar nil  - normalize chunk names (for new chunks created by program I guess)
:needs-mouse nil - 
:show-focus t - circles stuff in-focus on the window. Useful if using the UI for debugging
:trace-detail [low|medium|high] - how much stuff to print on screen, default medium

## Model run - a high-level view of what happens
- The program is run, displaying J (jameson) and 0 (a projectile)
- Initially, (P attend...) will be selected. It activates the visual-location module.
- This modules detects "a thing", produces a chunk: visual-location-0-0 with base infos about the "thing". This is where we would want to move-attention
- Once that is treated, the goal has been changed to attending state
- The (P respond...) is then launched. It's role is essentially to move the projectile forward. So whatever we do in terms of other production, at some point we would need to get to that production firing to move the projectile along.
- A press-key command is "simulated" by the production. That key is a cue for the program (the lisp program that runs the simulation) to move the projectile forward to the next location.
- The goal is cleared. We return to the initial attending state.
- Since the projectile has moved, we can now re-attend that new position and we go back to the first steps. A visual-location-2-0-0 chunk will be added to the DM with the new position details

## Visual  elements
The idea is to use unit 2's code to detect & encode projectiles posisitions.

# Bugs
## Can't launch additional runs
I ran the model a few times without issue (both with/without the window). After some launches, it seems I couldn't do additional runs. I just seemed to remain frozen after (jameson ....). I had to fully restart, including ACT-R. Not sure what caused it. I was playing with the model.lisp file but I'm pretty much I tried to run it with the default version on Git and it still hung.
- It may be that I somehow didn't shut it down properly and for some reason ACT-r couldn't restart it
