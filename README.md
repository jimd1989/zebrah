zebrah (extra h because there's probably already something out there called "zebra") is a BSD-licensed command line utility that creates [Zebra Puzzles](https://en.wikipedia.org/wiki/Zebra_Puzzle). I wrote it in a stateful type-annotated manner for performance. It can create puzzles interactively or use pre-written text files as input.

## Requirements

zebrah was written in [Chicken Scheme v. 5](https://call-cc.org/). Make sure you install it from your package manager.

## Installation

+ `make` (may have to be root to install Chicken extensions)
+ `make install` (may have to be root here too)
+ `make uninstall` (to remove)

## Usage

zebrah is invoked from the shell with:

    zebrah -options <file>

The options are:

+ `-v`: Verbose mode. Will show what the program is doing at any given time. This might be useful for larger puzzles, if only to reassure the user that it's still working.
+ `-i`: Interactive mode. Allows the user to specify the puzzle's fields one at a time through prompts. No file argument is required.
+ `-h`: Prints the help message.

Puzzle input files are just two-dimensional Lisp style lists, like:

    ((yellow blue red ivory green)
     (norway ukraine england spain japan)
     (water tea milk orange-juice coffee)
     (kool chesterfield old-gold lucky-strike parliament)
     (foxes horses snails dogs zebras))

The y and x values can differ, but make sure every row has the same number of individual fields. zebrah will generate random constraints based upon this input, then check it for any redundancies. The amount of assertions made about a puzzle state balloons as the dimensions increase, and checking can take a long time. The final output looks like:

    (5th = green)
    (green is right of ivory)
    (chesterfield = horses)
    (red = old-gold)
    (dogs is left of green)
    (lucky-strike is right of old-gold)
    (chesterfield is left of milk)
    (yellow is left of chesterfield)
    (coffee = green)
    (coffee is next to orange-juice)
    (ukraine is next to water)
    (spain is right of snails)
    (norway = water)
    (kool = water)
    (zebras = japan)

This information should be enough to create a puzzle that can be solved through inference (no searching), but contains no more clues than necessary. The exact number of constraints provided varies between invocations, so run it until you get something you find challenging. The logic of the puzzle is expressed in a rudimentary form so that the user can write the clues as he or she wishes. Automatically phrasing the output in plain English is an exercise left to the reader.

## Other info

+ There was tension between ensuring performance and accounting for all edge cases of the puzzle logic. It's possible that I've come up short in both of these endeavors. Redundant calculations are unavoidable to a certain extent, but if you notice an opportunity for a big shortcut, please let me know. Also contact me about omitted (or worse: contradictory!) inferences in the solver logic.
+ Sometimes it's fun when the puzzle only allows the user to infer things up to a certain point, then requires him or her to make educated guesses about what comes next. It would be nice if this program could also allow a sane degree of searching, but I don't even know where to begin with that.
+ It's possible to crash zebrah with the wrong input. I'll try to add more error checking in the future. Be reasonable about what you tell the program.
+ The function and struct names crib loosely from [CSP](https://en.wikipedia.org/wiki/Constraint_satisfaction_problem) terminology. I apologize if something here is not phrased with the theoretical rigor expected of a true scholar in the field.
