package dominik.blatt7

import global.Functor
import global.Monad

class User {
  val isDefined: Boolean = true
}

class DBConnection {
  def read(name: String): User = User()
}

val dbConn = DBConnection()

def isLegalUserImperativ(name: String): Boolean = {
  val user = dbConn.read(name) // Lese in globalem Kontext
  user.isDefined
}

def isLegalUserExplizit(name: String, dbConn: DBConnection): Boolean = {
  dbConn.read(name).isDefined
}

def isLegalUserGewuerzt(name: String): DBConnection => Boolean = {
  dbConn => dbConn.read(name).isDefined
}

class Reader[Z, A](val f: (Z) => A) {
  def this(a: A) = this((f: Z) => a)
  def apply(z: Z) = f(z)
  def map[B](g: A => B) = Reader(f andThen g)
  def flatMap[B](g: A => Reader[Z, B]): Reader[Z, B] = Reader(z => g(f(z))(z))
}

type UserDBReader[A] = Reader[DBConnection, A]

def isLegalUserExorziert(name: String): UserDBReader[Boolean] = {
  // von Reader wird hier nur Apply benoetigt.
  Reader((dbConnection: DBConnection) => dbConnection.read(name).isDefined) 
}
