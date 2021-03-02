package global

trait ContraFunctor[F[_]] {
  extension[A, B] (fb: F[B]) {
    def contraMap(f: A => B): F[A]
  }
}
