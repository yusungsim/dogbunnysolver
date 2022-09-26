package puzzle

import graph.{Graph}

// generates next possible states from given state 
case class Generator(graph: Graph, validr: Validator) {
  def genNextStates(state: State): List[State] = {
    state match {
      case State(b1, b2, dog) => {
        // first gen moving bunny1
        val nextBunny1Nodes = graph.getNextNodes(b1)
        // for connected next nodes of current bunny 1 node,
        val nextBunny1States: List[State] = 
          // filter only valid next nodes
          nextBunny1Nodes.filter(next => validr.checkBunny1(state)(b1, next))
          // then genrate states that moves bunny1 to that nodes.
            .map(next => state.moveBunny1(next))

        // second gen moving bunny2
        // similar logic
        val nextBunny2Nodes = graph.getNextNodes(b2)
        val nextBunny2States: List[State] = 
          nextBunny2Nodes.filter(next => validr.checkBunny2(state)(b2, next))
            .map(next => state.moveBunny2(next))

        // third gen moving dog
        // similar logiv
        val nextDogNodes = graph.getNextNodes(dog)
        val nextDogStates: List[State] = 
          nextDogNodes.filter(next => validr.checkDog(state)(dog, next))
            .map(next => state.moveDog(next))

        // aggregate all possible states
        nextBunny1States ++ nextBunny2States ++ nextDogStates
      }
    }
  }
}


