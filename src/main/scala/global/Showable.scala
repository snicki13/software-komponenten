package global

trait Showable[A] {
  def show(a: A): String
}
object Showable {
  def apply[A: Showable] = summon[Showable[A]]
}
