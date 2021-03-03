package nico.blatt_6

/*type Matrix[A] = Array[Array[A]]
// a[M][K] * b[K][N] = c[M][N]
def matMult[A: Numeric](a: Matrix[A], b: Matrix[A]) : Matrix[A] = {
    val numericA = summon[Numeric[A]]
    import numericA._
    val M = a.indices // M are indices of rows of Matrix a
    val N = b(0).indices // N are the indices of columns of Matrix b
    val K = a(0).indices // K is the number of rows of Matrix b and columns of
    matrix a
        assert(K.length == b.length) // a has as many rows as b has columns
    // for-Comprehension
    for ( i <- M 
}
val a: Matrix[Int] = Seq(
    Seq(1, 2, 3),
    Seq(4, 5, 6)
)
val b = Seq(
    Seq(7, 8),
    Seq(9, 10),
    Seq(11, 12)
)
val c = matMult(a, b)
println(c.map(_.mkString(",")).mkString("\n"))
// 58,64
// 139,154

@main def blatt6_1: Unit = {
    println(p_triples)
    println(p_triples_m)
}*/