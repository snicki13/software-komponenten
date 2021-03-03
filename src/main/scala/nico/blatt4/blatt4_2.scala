package nico

trait Showable[A] {
    def show(a: A): String
}
object Showable {
    def apply[A: Showable] = summon[Showable[A]]
}

//1
given Showable[String] with {
    def show(a: String): String = a
}
given Showable[Int] with {
    def show(a: Int): String = a.toString
}

//2
case class Pair[A](x: A, y: A)
given [T](using showT: Showable[T]): Showable[Pair[T]] with {
    def show(a: Pair[T]): String = Showable[T].show(a.x) + " " + Showable[T].show(a.y)
}

//3
given [T](using showT: Showable[T]): Showable[List[T]] with {
    def show(a: List[T]): String = {
        if (a.length > 1)
            Showable[T].show(a.head) + " " + Showable[List[T]].show(a.tail)
        else if (a.length > 0)
            Showable[T].show(a.head)
        else //Only if list is empty
            "" 
    }
}

//4
trait Functor[F[_]] {
    extension[A, B] (fa: F[A]) {
        def map(f: A => B): F[B]
    }
}
trait Listable[F[_]: Functor] {
    def toList[A](fa: F[A]) : List[A]
}
given [F[_], T](using listableF: Listable[F], showT: Showable[T]): Showable[F[T]] with {
    def show(ft: F[T]): String = Showable[List[T]].show(listableF.toList(ft))
}
// Test:
enum Tree[+A] {
    case Node(left: Tree[A], right: Tree[A])
    case Leaf(value: A)
}
import Tree._
given Functor[Tree] with {
    extension[A,B] (tree: Tree[A]) def map(f: A=> B): Tree[B] =
        tree match {
            case Leaf(value) =>
                Leaf(f(value))
            case Node(left, right) =>
                Node(left.map(f), right.map(f))
        }
}
given Listable[Tree] with {
    def toList[A](tree: Tree[A]) : List[A] =
        tree match {
            case Leaf(value) =>
                List(value)
            case Node(left, right) =>
                summon[Listable[Tree]].toList(left) ::: summon[Listable[Tree]].toList(right) //TODO apply?
        }
}

//5
//Geht

//6
trait ContraFunctor[F[_]] {
    extension[A, B] (fb: F[B]) {
        def contraMap(f: A => B): F[A]
    }
}
given ContraFunctor[Showable] with {
    extension[A, B] (showable: Showable[B]) {
        def contraMap(f: A => B): Showable[A] =
            new Showable[A] {
                def show(a: A): String = showable.show(f(a))
            }
    }
}

val p1 = Pair[String]("Hallo", "Welt")
val p2 = Pair(47, 11)
val p1AsString: String = Showable[Pair[String]].show(p1) // p1 als String
val p2AsString: String = Showable[Pair[Int]].show(p2) // p2 als String
val l1 = List("Hallo", "Welt")
val l2 = List(47, 11)
val str1: String = Showable[List[String]].show(l1) // l1 als String
val str2: String = Showable[List[Int]].show(l2) // l2 als String*/

val tree = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
val str4: String = Showable[Tree[Int]].show(tree) // [[1,2,3,4]]
val tree5 = Node(Leaf(Pair(1, 2)), Node(Node(Leaf(Pair(3,4)), Leaf(Pair(5,6))), Leaf(Pair(7,8))))
val str5: String = Showable[Tree[Pair[Int]]].show(tree5)

val f1 = 42.123
val func = (d: Double) => d.toInt
val showableDouble: Showable[Double] = summon[Showable[Int]].contraMap(func)
val str6: String = showableDouble.show(f1)

@main def blatt4_2: Unit = {
    println("3:")
    println(str1)
    println(str2)
    println("4:")
    println(str4)
    println("5:")
    println(str5)
    println("6:")
    println(str6)
}