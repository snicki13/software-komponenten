package global

enum Tree[+A] {
  case Node(left: Tree[A], right: Tree[A])
  case Leaf(value: A)
}