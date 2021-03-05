package dominik.blatt8

def nextRandomFun(r_alt: Int): (Int, Int) = {
  val a = 48247L // Primzahl
  val c = 11L // Primzahl
  val m = (1L << 31) -1
  val r_neu = ((a*r_alt + c) % m).toInt
  (r_neu, r_neu)
}
def quicksortFun(lst: List[Int]): List[Int] = lst match {
  case Nil => Nil
  case _ :: Nil => lst
  case first :: _ =>
    val pivot = first
    val l1 = lst.filter( _ < pivot)
    val l2 = lst.filter( _ == pivot)
    val l3 = lst.filter( _ > pivot)
    quicksortFun(l1) ++ l2 ++ quicksortFun(l3)
}

// Berechnung die Zufallszahlen nutzt
@main def testQuicksortFun(): List[Int] = {
  val initialSeed_fun: Int = 1234567890
  val (a0, r0) = nextRandomFun(initialSeed_fun)
  val (a1, r1) = nextRandomFun(r0)
  val (a2, r2) = nextRandomFun(r1)
  val (a3, r3) = nextRandomFun(r2)
  val lst = List(a0, a1, a2, a3)
  quicksortFun(lst)
}
