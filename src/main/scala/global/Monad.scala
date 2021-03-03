package global

trait Monad[F[_]] extends Functor[F] {
  def pure[A] (x: A): F[A]
  extension[A, B] (x: F[A]) {
    def flatMap(f: A=> F[B]): F[B]
    def map(f: A=> B) = x.flatMap(f.andThen(pure))
  }
}
object Monad {
  def apply[F[_]: Monad] = summon[Monad[F]]
}