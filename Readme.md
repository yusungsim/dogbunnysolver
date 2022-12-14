# DogBunnyPuzzle Solver

Automated solver to the [Dog Bunny Puzzle](http://www.dogbunnypuzzle.com/).

# How to run

1. Install [sbt](https://www.scala-sbt.org/)
2. Run the solver with sbt's `run` command.
i.e. run following command from the project top directory.
`sbt run`
3. Interpret the solution.
Each line in the solution describes the puzzle state. (i.e. where bunnies and a dog is)
Read each line, and move bunny or dog as the line says.

# Concepts

Node == puzzle state == a possible configuration of the dog and bunnies in the puzzle.

Directed edge == state transformation == a possible move from one puzzle state to another.

Enumerate possible states from previous to next and reach the goal state. When goal state reached, backtrack to produce the solution trace.

# Todos

- [ ] Modularization of the code

- [ ] Optimization: always find the optimal solution

- [ ] Better descripttion for the solution (i.e. sequence of "move sth. from sth. to sth.")

- [ ] Getting the puzzle information with serialized format
