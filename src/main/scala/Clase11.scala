
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[F: Semigroup] = implicitly[Semigroup[F]]
}

trait SemigroupInstances {

  implicit def intSG = new Semigroup[Int] {
    def combine(x: Int, y: Int): Int = x + y
  }

  implicit def optSG[A: Semigroup] = new Semigroup[Option[A]] {
    def combine(x: Option[A], y: Option[A]): Option[A] =
      (x, y) match {
        case (Some(a), Some(b)) =>
          Option(Semigroup[A].combine(a, b))
        case (a @ Some(_), _) => a
        case (_, b @ Some(_)) => b
        case _                => None
      }
  }

  trait SemigroupSyntax {
    //La clase implicita agrega funciones donde no las habia
    implicit class Semigroupsyntax[A](a: A) {
      def combine(b: A)(implicit s: Semigroup[A]): A =
        s.combine(a, b)
    }
  }

  object SemigroupOps extends SemigroupInstances with SemigroupSyntax

  import SemigroupOps._

  val one: Option[Int] = Some(1)
  val none: Option[Int] = None

  one combine one
  one combine none
  none combine none

}

object Clase11 {

  trait Num[A] {
    def combine(a: A, b: A): A
  }

  object Ops {

    implicit object Int extends Num[Int] {
      override def combine(a: Int, b: Int): Int = a + b
    }

    implicit object OptInt extends Num[Option[Int]] {
      override def combine(a: Option[Int], b: Option[Int]): Option[Int] = {
        (a, b) match {
          case (Some(x), Some(y)) => Some(x + y)
          case (Some(x), None)    => Some(x)
          case (None, Some(x))    => Some(x)
          case (None, None)       => None
        }
      }
    }
  }

  import Ops._

  def combine[T](a: T, b: T)(implicit s: Num[T]): T =
    s.combine(a, b)

  val one: Option[Int] = Some(1)
  val none: Option[Int] = None

  combine(1, 1) shouldBe 2
  combine(one, none) shouldBe Some(1)
  combine(none, none) shouldBe None
  combine(one, one) shouldBe Some(1)


}
