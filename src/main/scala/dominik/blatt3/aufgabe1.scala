package dominik.blatt3

import global._
import global.Tree._

given Functor[Tree] with {
  import Tree._
   extension[A, B](fa: Tree[A]) def map(f: A => B): Tree[B]  = fa match {
    case Node(left, right) => Node(left.map(f), right.map(f))
    case Leaf(value) => Leaf(f(value))
  }
}

@main def aufgabe_1: Unit = {
  val tree_1 = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
  val tree_2 = tree_1.map(_ * 3)
  print(tree_2)
}
