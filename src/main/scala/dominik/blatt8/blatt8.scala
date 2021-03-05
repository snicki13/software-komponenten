package dominik.blatt8

val initialSeed: Int = 1234567890
var r_n: Int = initialSeed
def nextRandom(): Int = {
  // ohne viel Nachdenken
  val a = 48247L // Primzahl
  val c = 11L// Primzahl
  val m = (1L << 31) -1
  r_n = ((a*r_n + c) % m).toInt
  r_n
}
def quicksort(lst: List[Int]): List[Int] = lst match {
  case Nil => Nil
  case _ :: Nil => lst
  case first :: _ => 
    val pivot = first 
    val l1 = lst.filter( _ < pivot)
    val l2 = lst.filter( _ == pivot)
    val l3 = lst.filter( _ > pivot)
    quicksort(l1) ++ l2 ++ quicksort(l3)
}

// Berechnung die Zufallszahlen nutzt
 @main def testQuicksort(): List[Int] = {
   val a0 = nextRandom()
   val a1 = nextRandom()
   val a2 = nextRandom()
   val a3 = nextRandom()
   val lst = List(a0, a1, a2, a3)
   quicksort(lst)
 }
