package dominik.blatt8

case class State[A, S](run: S=>(A, S)) {
  def map[B](f: A => B): State[B, S] = State(s => {
    val (a, newState) = run(s)
    (f(a), newState)
  })
  def flatMap[B](f: A => State[B, S]): State[B, S] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def pure[A, S](a: A): State[A, S] = State(s => (a, s))
}


def nextRandomMonad: State[Int, Int] = {
  State(r => {
    val a = 17L // Primzahl
    val c = 11L // Primzahl
    val m = (1L << 31) -1
    val r_neu = ((a * r + c) % m).toInt
    (r_neu, r_neu)
  })
}

def quicksortMonad(lst: List[Int]): List[Int] = lst match {
  case Nil => Nil
  case _ :: Nil => lst
  case first :: _ =>
    val pivot = first
    val l1 = lst.filter( _ < pivot)
    val l2 = lst.filter( _ == pivot)
    val l3 = lst.filter( _ > pivot)
    quicksortMonad(l1) ++ l2 ++ quicksortMonad(l3)
}

// Berechnung die Zufallszahlen nutzt
def testQuicksortMonad(): State[List[Int], Int] = {
  for (
    a0 <- nextRandomMonad;
    a1 <- nextRandomMonad;
    a2 <- nextRandomMonad;
    a3 <- nextRandomMonad
  ) yield quicksortMonad(List(a0, a1, a2, a3))
}

@main def testMonadicSort() = {
  val initialSeed: Int = 5
  val res = testQuicksortMonad().run(initialSeed)._1
  println(res)
}