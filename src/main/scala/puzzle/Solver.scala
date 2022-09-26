package puzzle

// solves the puzzle with Validator and Generator
case class Solver(check: Validator, gen: Generator){
  def apply(init: State, goal: State): List[State] = {
    val initQueue = List((init, 0))
    val rawsol = solveWithQueue(goal, initQueue, 0)
    init :: rawsol.reverse
  }

  def backtrack(queue: List[(State, Int)], id: Int): List[State] = {
    queue(id) match {
      case (curState, prevId) => {
        if (prevId == 0) {
          List(curState)
        } else {
          curState :: backtrack(queue, prevId) 
        }
      }
    }
  }

  // queue-based naive solver
  def solveWithQueue(
    goal: State, 
    // list-based queue. 
    queue: List[(State, Int)], 
    // index of head of queue. 
    head: Int
  ): List[State] = {
    // pops a state, check if it's a goal state
    queue(head) match {
      case (curState, _) => {
        // if curState is goal, end the search.
        // backtrack to produce the actual trace
        if (curState == goal) {
          backtrack(queue, head)
        // if curState is not a goal,
        // generate next states and add to the queue.
        // then continue solving with next queue element.
        // should avoid adding redundent states
        } else {
          val nextStates = gen.genNextStates(curState)
          val newQueue = 
            queue ++ nextStates
              .filter(x => {
                val queueOnlyStates = queue.map({ case (s, i) => s })
                !(queueOnlyStates contains x)
              })
              .map(x => (x, head))
          solveWithQueue(goal, newQueue, head+1) // todo: TCO
        }
      }
    }
  }
}


