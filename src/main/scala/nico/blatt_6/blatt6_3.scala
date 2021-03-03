package nico.blatt_6

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Success, Failure}

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
given Monad[List] with {
    def pure[A](x:A): List[A] = List(x)
    extension[A, B] (x: List[A]) {
        def flatMap(f: A=> List[B]): List[B] = x.flatMap(f)
        override def map(f: A=> B) = x.map(f)
    }
}

type ID[A] = A
given idMonad: Monad[ID] with {
    def pure[A](a: A): ID[A] = a
    extension [A, B](fa: ID[A]) {
        def flatMap(f: A => ID[B]): ID[B] = f(fa)
        override def map(f: A => B): ID[B] = fa.flatMap(f.andThen(pure))
    }
}

def sumSquare[F[_]: Monad, N: Numeric](a: F[N], b: F[N]): F[N] = {
    val numeric = summon[Numeric[N]]
    import numeric._
    for {
        x <- a
        y <- b
    } yield x*x + y*y
}

given futureMonad: Monad[Future] with {
    def pure[A](x:A): Future[A] = Future(x)
    extension[A, B] (xs: Future[A]) {
        def flatMap(f: A=> Future[B]): Future[B] = xs.flatMap(f)
        override def map(f: A=> B) = xs.map(f)
    }
}

val sq_f: Future[Double] = sumSquare( Future(2.0), Future(3.0) )

// Iteration u¨ber Listen
val sq_list = sumSquare(List(2.0), List(3.0)) // List(13)
// implizite Konversion A => ID[A]
given toID[A]: Conversion[A, ID[A]] with {
    def apply(a: A): ID[A] = a
}
// "Iteration" u¨ber einzelne Werte
val sq_id = sumSquare(2.0, 3.0) // 13

@main def blatt6_3: Unit = {
    println(sq_list)
    println(sq_id)

    sq_f.onComplete {
        case Success(result) => println(result)
        case Failure(failure) => println("Failed because of " + failure)
    }
}
