import scala.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global


object Clase8 {

  val fut1 = Future {
    println(s"Futuro 1")
    1
  }
  val fut2 = Future {
    println(s"Futuro 2")
    2
  }
  val fut3 = Future {
    println(s"Futuro 3")
    3
  }

  println("inicio for")
  val result = for{
    a <- fut1
    b <- fut2
    c <- fut3
  }yield {
    println("suma dentro del for")
    a + b + c
  }
  println("final for")
  val _ = Thread.sleep(200)
  println(result)

  case class id(num: Int)
  case class Name(name: String)
  case class LastName(lastName: String)
  case class User(id: id, name: Name, lastName: LastName)
  object User { def apply(id: id, name: Name, lastName: LastName): User =
      new User(id, name, lastName)
  }

  def getId: Future[id] = {
    Future(id(111))
  }
  def getName: Future[Name] = {
    Future(Name("Maria"))
  }
  def getLastName:Future[LastName] = {
    Future(LastName("Ocampo"))
  }

}
