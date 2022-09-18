# DogBunnyPuzzle Solver

Automated solver to the [Dog Bunny Puzzle](http://www.dogbunnypuzzle.com/).

# How to run

1. Install [sbt](https://www.scala-sbt.org/)
2. Run the solver with sbt's `run` command.
i.e. run following command from the project top directory.
`sbt run`
3. Interpret the solution.
Each line in the solution describes the puzzle state.
Read each line, and move bunny or dog as the line says.

# Concepts

Node == puzzle state == a possible configuration of the dog and bunnies in the puzzle.

Directed edge == state transformation == a possible move from one puzzle state to another.

Find solution by finding path from the initial state from final state.

