import scala.util.{Success, Try}

object Clase12 {

  trait Monad[F[_]] {
    def pure[A]: A => F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
  object monadInstances {
    implicit def optionMonad = new Monad[Option] {
      override def pure[A]: A => Option[A] = Option.apply

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        fa flatMap f
    }
    implicit def tryMonad = new Monad[Try] {
      override def pure[A]: A => Try[A] = Success(_)
      override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
        fa flatMap f
    }
    type either[A] = Either[String, A]
    implicit def eitherMonad = new Monad[either] {
      override def pure[A]: A => either[A] = Right.apply

      override def flatMap[A, B](fa: either[A])(f: A => either[B]): either[B] =
        fa flatMap f
    }
  }
  object monadValidations {
    def leftIdentity[F[_], A](x: A)(f: A => F[A])(implicit m: Monad[F]): Boolean =
      m.flatMap(m.pure(x))(f) == f(x)
    def rightIdentity[F[_], A](x: A)(implicit m: Monad[F]) =
      m.flatMap(m.pure(x))(m.pure) == m.pure(x)
    def associativity[F[_], A](x: A)(f: A => F[A])(g: A => F[A])(
      implicit m: Monad[F]) =
      m.flatMap(m.pure(x))(_ => m.flatMap(f(x))(g)) == m.flatMap(
        m.flatMap(m.pure(x))(f))(g)
  }
  object monadOps {
    def pure[F[_], A](a: A)(implicit m: Monad[F]): F[A] = m.pure(a)
    def flatMap[F[_], A, B](fa: F[A])(f: A => F[B])(implicit m: Monad[F]): F[B] =
      m.flatMap(fa)(f)
  }

}
