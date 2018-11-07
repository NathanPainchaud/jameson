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

## Running experiments

Once the Lisp is properly configured and the model is loaded, two functions defined in the program are designed to be called by the user.

For the sake of clarity, we will define a trial as _an experiment that lasts from the generation of the projectiles to the projectiles intersecting with Jameson's movement axis_.

Depending on which of the two functions the user calls, the program can run for a single trial or multiptle trials.

### Execute a single trial

To execute a single trial, simply call the function `jameson-trial`.

The `jameson-trial` function can be run without specifying any parameters. However, it allows for two named parameters, described below:
- `visible` (optional, default `t`) Indicates to display a window (`t`) or not (`nil`) when running an experiment. This parameter also affects the execution time of the program. When a window is displayed, experiments are run in real-time, while they are run in the simulated time of the ACT-R model otherwise.
- `projectiles-nb` (optional, default `1`) The number of projectiles directed at Jameson at each trial.

When specifying values for the parameters, a call to the `jameson-trial` function with the default values would look like this:
```lisp
(jameson-trial :visible t :projectiles-nb 1)
```

### Execute multiple trials
To execute multiple trials, simply call the function `jameson`.

The `jameson` function takes a minimum of one parameter to run. However, it allows for two more named parameters. All parameters are described below:
- `n` (required) The number of trials to run in the experiment.
- `visible` (optional, default `nil`) Indicates to display a window (`t`) or not (`nil`) when running an experiment. This parameter also affects the execution time of the program. When a window is displayed, experiments are run in real-time, while they are run in the simulated time of the ACT-R model otherwise.
- `projectiles-nb` (optional, default `1`) The number of projectiles directed at Jameson at each trial.

When specifying values for the parameters, a call to the `jameson` function for 100 trials with the default values would look like this:
```lisp
(jameson 100 :visible nil :projectiles-nb 1)
```