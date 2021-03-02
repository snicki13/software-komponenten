package global

trait Listable[F[_]: Functor] {
  def toList[A](fa: F[A]) : List[A]
}
