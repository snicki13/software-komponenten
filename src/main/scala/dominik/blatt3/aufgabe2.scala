import global._
import global.Tree._

given Showable[String] with {
  override def show(a: String): String = a
}

given Showable[Int] with {
  override def show(i: Int): String =  i.toString()
}

// Showable Pair
given [T](using showT: Showable[T]): Showable[Pair[T]] with {
  override def show(p: Pair[T]): String = s"${showT.show(p.x)}, ${showT.show(p.y)}"
}

// Showable List
given [T](using showT: Showable[T]): Showable[List[T]] with {
  override def show(l: List[T]): String = l.map(showT.show).mkString(", ")
}

given Functor[Tree] with {
  import Tree._
  extension[A, B](fa: Tree[A]) {
    def map(f: A => B): Tree[B] = fa match {
      case Node(left, right) => Node(left.map(f), right.map(f))
      case Leaf(value) => Leaf(f(value))
    }
  }
}

given Listable[Tree] with {
  def toList[A](tree: Tree[A]) : List[A] = {
    tree match {
      case Node(left, right) => toList(left) ::: toList(right)
      case Leaf(value) => List(value)
    }
  }
}

// Listable
given [F[_], T] (using listableF: Listable[F], showT: Showable[T]): Showable[F[T]] with {
  def show(ft: F[T]): String = listableF.toList(ft).map(showT.show).mkString(", ")
}

given ContraFunctor[Showable] with {
  extension[A, B](showable: Showable[B]) {
    def contraMap(f: A => B): Showable[A] = new Showable[A] {
      def show(a: A): String = showable.show(f(a))
    }
  }
}

@main def aufgabe2_1: Unit = {
  val p1 = Pair[String]("Hallo", "Welt")
  val p2 = Pair(47, 11)

  val p1AsString: String = Showable[Pair[String]].show(p1)
  val p2AsString: String = Showable[Pair[Int]].show(p2)

  val l1 = List[String]("Hallo", "Welt")
  val l2 = List(47, 11)

  val l1AsString: String = Showable[List[String]].show(l1)
  val l2AsString: String = Showable[List[Int]].show(l2)
  println(p1AsString)
  println(p2AsString)
  println(l1AsString)
  println(l2AsString)
}

@main def aufgabe2_4: Unit = {
  val tree = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
  val str: String = Showable[Tree[Int]].show(tree)
  print(str)
}

@main def aufgabe2_5: Unit = {
  // Antwort: Keine, da ein Showable zu Pair existiert
  val tree = Node(Leaf(Pair(1, 2)), Node(Node(Leaf(Pair(3,4)),Leaf(Pair(5,6))), Leaf(Pair(7,8))))
  val str: String = Showable[Tree[Pair[Int]]].show(tree)
  print(str)
}

@main def aufgabe2_6: Unit = {
  val tree = Node(Leaf(Pair(1, 2)), Node(Node(Leaf(Pair(3, 4)), Leaf(Pair(5, 6))), Leaf(Pair(7, 8))))
  val str: String = Showable[Tree[Pair[Int]]].show(tree)
  print(str)
}