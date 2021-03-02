package nico.blatt1

//Aufgabe 2

trait Monoid[A] {
  def unit: A
  extension (x: A) {
    def combine(y: A):A
  }
}
object Monoid {
  def apply[A: Monoid] = summon[Monoid[A]]
}

given intmon: Monoid[Int] with {
  //def combine(x: Int, y: Int): Int = x+y
  extension (x: Int) {
    def combine(y: Int):Int = x+y
  }
  def unit: Int = 0
}
given stringmon: Monoid[String] with {
  extension (x: String) {
    def combine(y: String):String = x+y
  }
  def unit: String = ""
}
given optmon[A:Monoid]: Monoid[Option[A]] with {
  extension (x: Option[A]) {
    def combine(y: Option[A]): Option[A] = (x, y) match {
      case (None, Some(b)) => Some(b)
      case (Some(a), None) => Some(a)
      case (Some(a), Some(b)) => Some(a.combine(b))
    }
  }
  def unit: Option[A] = None
}

def sum[M: Monoid](lst: List[M]): M = lst.foldLeft(Monoid[M].unit)( (acc, item) => acc.combine(item))
val loi: List[Option[Int]] = List(Some(1), Some(2), None, Some(3))
val los: List[Option[String]] = List(Some("1"), Some("2"), None, Some("3"))

val sloi = sum(loi)
val slos = sum(los)


@main def hello: Unit = {
  println(sloi)
  println(slos)
}
