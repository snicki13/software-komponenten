package dominik

  
  def perms[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => List(Nil)
    case head:: tail => {
      for (
        permsOfRest <- perms(lst.tail);
        headInpermsOfRest <- inserts(lst.head, permsOfRest)
      ) yield headInpermsOfRest
    }
  }
  
  def inserts[A](x: A, lst: List[A]): List[List[A]] = lst match {
    case Nil => List(List(x))
    case head :: tail => 
      (x :: lst) :: (
        for(
          xInRest <- inserts[A](x, lst.tail)
        ) yield lst.head :: xInRest
      )
  }

  @main def test: Unit = {
    val ps = perms("abc".toList)
      .map(_.mkString(""))
      .mkString(", ")
    println(ps)
  }
