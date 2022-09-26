package puzzle

import graph.Node

// abstraction of puzzle state
case class State(b1: Node, b2: Node, dog: Node) {
  def moveBunny1(n: Node): State = State(n, b2, dog)    
  def moveBunny2(n: Node): State = State(b1, n, dog)    
  def moveDog(n: Node): State = State(b1, b2, n)    
  def print = println(s"Bunny1: ${b1.name} | Bunny2: ${b2.name} | Dog: ${dog.name}")
}
