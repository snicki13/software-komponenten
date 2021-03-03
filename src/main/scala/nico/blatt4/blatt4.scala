package nico
    
enum Tree2[+A] {
    case Node(left: Tree2[A], right: Tree2[A])
    case Leaf(value: A)
}
import Tree2._

trait Functor2[F[_]] {
    extension[A,B] (fa: F[A]) {
        def map(f: A=> B): F[B]
    }
}

given Functor2[Tree2] with {
    extension[A,B] (tree: Tree2[A]) def map(f: A=> B): Tree2[B] =
        tree match {
            case Leaf(value) =>
                Leaf(f(value))
            case Node(left, right) =>
                Node(left.map(f), right.map(f))
        }
}

val tree_1 = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
val tree_2 = tree_1.map( _*3 )


@main def blatt4: Unit = {
    println(tree_1)
    println(tree_2)
}