package nico.blatt_6

/*
trait Functor[F[_]] {
    extension[A, B] (fa: F[A]) {
        def map(f: A => B): F[B]
    }
}
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

given Monad[Seq] with {
    def pure[A](x:A): Seq[A] = Seq(x)
    extension[A, B] (x: Seq[A]) {
        def flatMap(f: A=> Seq[B]): Seq[B] = x.flatMap(f)
       override def map(f: A=> B) = x.map(f)
    }
}*/

type Triple = (Int, Int, Int)
val l1 = (1 to 10)
val l2 = (1 to 10)
val l3 = (1 to 10)
val p_triples_m : Seq[Triple] = 
    l1.flatMap(i => 
        l2.flatMap(j =>
            l3.flatMap(k => {
                if (i*i + j*j == k*k)
                    Seq((i,j,k))
                else
                    Nil
            })))
val p_triples: Seq[Triple] =
    for (
        i <- 1 to 10;
        j <- 1 to 10;
        k <- 1 to 10;
        if i*i + j*j == k*k
    ) yield (i, j, k)


@main def blatt6_1: Unit = {
    println(p_triples)
    println(p_triples_m)
}
