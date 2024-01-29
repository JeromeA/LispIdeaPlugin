# Comparing LispIdeaPlugin plugin with SLT.

## REPL

SLT uses 3 tabs:
* standard output, which contains all outputs from the lisp process. That's the startup messages, the output from the
code being run, the REPL prompt, and the results of the evaluation.
* error output, which contains all the errors from the lisp process.
* Slime log, which contains all the communication between SLT and the lisp process.

LispIdeaPlugin uses a single pane, which contains interaction panels. Each panel contains:
* one input expression
* one output expression
* optionally, one error subpane.
