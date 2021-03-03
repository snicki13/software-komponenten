package nico

trait Functor_Test[F[_]] {
    extension[A, B] (fa: F[A]) {
        def map(f: A => B): F[B]
    }
}
case class ToInt[A](fa: A => Int) {
    def apply(a: A) = fa(a)
}
/*given Functor_Test[ToInt] with {
    extension[A, B] (fa: A => Int) {
        def map(f: A => B): B => Int = b => 4711
    }
}*/
given Functor_Test[ToInt] with {
    extension[A, B] (fa: ToInt[A]) {
        def map(f: A => B): ToInt[B] =  ToInt[B](b => 4711) //Setzt alle werte auf 4711, bööööse!
    }
}
def f(str: String): Int = str.length
def g(i: Int): Char = i.toString.charAt(0)
val x: ToInt[String] = ToInt( (str: String) => str.length )
val xMap_f: ToInt[Int] = x.map(f)
val xMap_f_Map_g: ToInt[Int] = xMap_f.map(g)
val xMap_fg: ToInt[Int] = x.map(f andThen g)

@main def blatt4_3: Unit = {
    assert( xMap_f_Map_g(42) == xMap_fg(42) )
    //identity funktion funktioniert nicht!!!
}

