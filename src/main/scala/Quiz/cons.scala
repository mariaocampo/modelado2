package Quiz

import scala.annotation.tailrec

sealed trait Collection[+A]
case object Empty extends Collection[Nothing]
final case class Cons[+A](head: A, tail: Collection[A]) extends Collection[A]

object Collection {
  def apply[A](as: A*): Collection[A] = as.toList match {
    case cons if cons.isEmpty => Empty
    case h :: t => Cons(h, apply(t: _*))
  }

  def foldRight[A, B](col: Collection[A], z: B)(f: (A, B) => B): B = col match {
    case Empty => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def length(col: Collection[Int]): Int = foldRight(col, Collection.length(col)) { ??? }
}

object CollectionInt {
  def sumFold(col: Collection[Int]): Int = Collection.foldRight(col, Collection.length(col))(???)

  def sumRec(col: Collection[Int]) = {
    @tailrec
    def sumAccumulator(col: Collection[Int]): Int = {
      col match {
        case h :: t => h + sumAccumulator(t)
        case Nil => 0
      }
    }
  }

  def productFold(col: Collection[Int]) = ???

  def productRec(col: Collection[Int]): Int = {
    @tailrec
    def productAccumulator(col: Collection[Int]): Int = {
      col match {
        case h :: t => h * productAccumulator(t)
        case Nil => 0
      }
    }
  }

  def maxFold(col: Collection[Int]) = Collection.foldRight(col, Integer.MIN_VALUE)(_ max _)

  def maxRec(col: Collection[Int]): Int = ???
}
