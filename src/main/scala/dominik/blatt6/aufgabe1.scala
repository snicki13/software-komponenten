package dominik.blatt6

  type Triple = (Int, Int, Int)

@main def aufgabe6_1_1(): Unit = {
  val p_triples: Seq[Triple] =
    for (
      i <- 1 to 10;
      j <- 1 to 10;
      k <- 1 to 10;
      if i*i + j*j == k*k
    ) yield (i, j, k)
    
  val p_triples_haesslich: Seq[Triple] = {
    (1 to 10).flatMap(i => {
      (1 to 10).flatMap(j => {
        (1 to 10).flatMap(k => {
          if (i * i + j * j == k * k) Seq((i, j, k)) else Seq()
          })
      })
    })
  }

  println(p_triples)
  println(p_triples_haesslich)
}

@main def aufgabe6_1_2(): Unit = {
  
  def subseq3[A](lst: Seq[A]): Seq[Seq[A]] = {
    for (
      i <- 0 to lst.length - 1;
      j <- i + 1 to lst.length - 1;
      k <- j + 1 to lst.length - 1
    ) yield Seq(lst(i), lst(j), lst(k))
  }
  
  val w = subseq3(Seq("a", "b", "c", "d", "e"))
  
  println(
    w.map(_.mkString(", ")).mkString("\n")
  )

}

@main def aufgabe6_1_3(): Unit = {

  def subseqn[A](lst: Seq[A], length: Int): Seq[Seq[A]] = {
    
    def iterate(i: Int, j: Int, indices: Seq[Int]): Seq[Seq[Int]] = {
      if (i == length) {
        Seq(indices) 
      } else {
        val res = (j to lst.length -1 ).flatMap(k => iterate(i+1, j+1, indices ++ Seq(j)))
        res
      }
    }
    iterate(0, 0, Seq()).map( _.map( lst(_)))
  }

  val w = subseqn(Seq("a", "b", "c", "d", "e"), 4)

  println(
    w.map(_.mkString(", ")).mkString("\n")
  )

}