package nico.blatt_6

trait ErrorMonad[F[_], E] {
    def error[A](e: E): F[A]
    def pure[A](x: A): F[A]
    extension [A, B] (x: F[A]) {
        def map(f: A => B): F[B]
        def flatMap(f: A => F[B]): F[B]
        //def ensure(p: A => Boolean, orElse: E): F[A]
    }
}

type StringErrorMonad[G[_]] = ErrorMonad[G, String]
object StringErrorMonad {
    def apply[F[_]: StringErrorMonad] = summon[StringErrorMonad[F]]
}

enum Exp {
    case Const(v: Int)
    case Add(t1: Exp, t2: Exp)
    case Sub(t1: Exp, t2: Exp)
    case Mult(t1: Exp, t2: Exp)
    case Div(t1: Exp, t2: Exp)
}
import Exp._
def eval[F[_]: StringErrorMonad](e: Exp): F[Int] = e match {
    case Const(v: Int) => StringErrorMonad[F].pure(v)
    case Add(l, r) =>
        for (
            v1 <- eval(l);
            v2 <- eval(r)
        ) yield v1 + v2
    case Sub(l, r) =>
        for (
            v1 <- eval(l);
            v2 <- eval(r)
        ) yield v1 - v2
    case Mult(l, r) =>
        for (
            v1 <- eval(l);
            v2 <- eval(r)
        ) yield v1 * v2
    case Div(l, r) => // etwas umständlich da Either nicht filterbar ist
        eval(l).flatMap((v1: Int) =>
            eval(r).flatMap((v2: Int) =>
                if (v2 != 0) StringErrorMonad[F].pure(v1 / v2) else StringErrorMonad[F].error("Divide by zero")
            )
        )
}

type StringOrDunno[A] = Either[String, A]
given ErrorMonad[StringOrDunno, String] with {
    def pure[A](x: A): Either[String, A] = Right(x)
    def error[A](x:String):  Either[String, A] = Left(x)
    extension [A, B](xs: Either[String, A]) {
        def flatMap(f: A => Either[String, B]): Either[String, B] =
            xs.flatMap(f)
        override def map(f: A => B) =
            xs.map(f)
    }
}

//2
/*trait EvaluationError[F] {
    def value: F
}
object EvaluationError {
    def apply[F: EvaluationError] = summon[EvaluationError[F]]
}
given EvaluationError[String] with {
    def value: String = "Test"
}

def eval2[F[_], E: EvaluationError](e: Exp)(using fe: ErrorMonad[F, E]): F[Int] = e match {
    case Const(v: Int) => fe.pure(v)
    case Add(l, r) =>
        for (
            v1 <- eval2(l);
            v2 <- eval2(r)
        ) yield v1 + v2
    case Sub(l, r) =>
        for (
            v1 <- eval2(l);
            v2 <- eval2(r)
        ) yield v1 - v2
    case Mult(l, r) =>
        for (
            v1 <- eval2(l);
            v2 <- eval2(r)
        ) yield v1 * v2
    case Div(l, r) => // etwas umständlich da Either nicht filterbar ist
        eval2(l).flatMap((v1: Int) =>
            eval2(r).flatMap((v2: Int) =>
                if (v2 != 0) fe.pure(v1 / v2) else fe.error("Divide by zero")
            )
        )
}*/

val exp1: Exp =
    Add(Const(18),
        Div(
            Mult(Const(12), Const(4)),
            Const(0)))
val exp2: Exp =
    Add(Const(18),
        Div(
            Mult(Const(12), Const(4)),
            Const(1)))
val res1 = eval(exp1)
val res2 = eval(exp2)
@main def blatt6_4: Unit = {
    println(res1)
    println(res2)
}
