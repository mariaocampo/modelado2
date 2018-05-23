import scala.annotation.tailrec

object Clase3 {

  def fact0: PartialFunction[Int, Int] = { case 0 => 1}
  def factR: PartialFunction[Int, Int] = {case n => n * factR(n-1)}

  def factC: Int => Int = fact0 orElse(factR)
  
  def fact(x: Int): Int = {
    def factLoop(n: Int, acc: Int): Int = {
      if (n <= 0) acc else factLoop(n - 1, n * acc)
    }
    factLoop(x,1)
  }

  def mcd(x: Int, y: Int): Int = {
    @tailrec
    def func(x: Int, y: Int): Int = {
      if (y <= 0) x else func(y, x % y)
    }
    func(x, y)
  }

}
