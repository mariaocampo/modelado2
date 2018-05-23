import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Clase4 {

  def sumElemen(li: List[Int]): List[Int] = {
    li match {
      case h :: Nil => (h + 1) :: Nil
      case h :: t   => (h + 1) :: sumElemen(t)
      case _        => Nil
    }
  }

  def sumSafe(l: List[Int]): List[Int] = {
    @tailrec
    def sumElemenSafe(elm: List[Int], acc: List[Int]): List[Int] = {
      elm match {
        case h :: Nil => acc :+ (h + 1)
        case h :: t   => sumElemenSafe(t, acc :+ (h + 1))
        case Nil      => acc
      }
    }
    sumElemenSafe(l, Nil)
  }

  def prom(l: List[Double]): Double = {
    @tailrec
    def promSafe(elm: List[Double], acc: Double, size: Double): Double = {
      elm match {
        case h :: t => promSafe(t, h + acc, size + 1)
        case Nil    => acc / size
      }
    }
    l match {
      case Nil => 0.0
      case _   => promSafe(l, 0.0, 0.0)
    }
  }

  case class Mensaje(texto: String, sha: Int)
  object Mensaje {
    def apply(texto: String, sha: Int): Option[Mensaje] = {
      if (validarSha(sha)) Option(new Mensaje(texto, sha))
      else None
    }

    private def validarSha(sha: Int): Boolean = ???
  }

  case class Mensaje2(texto: String, sha: Int)
  object Mensaje2 {
    def apply(texto: String, sha: Int): Try[Mensaje2] = {
      if (validarSha(sha)) Success(new Mensaje2(texto, sha))
      else Failure(new Exception)
    }
    private def validarSha(sha: Int): Boolean = ???
  }

  val cc = for {
    r <- Left(1)
    c <- Right(3)
  } yield c + c

  trait MensajeError { val error: String }
  case class InvalidSha(error: String = "Invalid Sha") extends MensajeError
  case class InvalidText(error: String = "Invalid Text") extends MensajeError

  sealed trait Estado
  trait Cifrado extends Estado
  trait Plano extends Estado

  case class Mensaje3[S <: Estado](texto: String, sha: Int)
  object Mensaje3 {
    def apply(texto: String,
              sha: Int): Either[MensajeError, Mensaje3[Plano]] = {
      for {
        sha <- validarSha(sha)
        text <- validarTexto(texto)
      } yield new Mensaje3[Plano](text, sha)
    }
    private[this] def validarSha(sha: Int): Either[MensajeError, Int] = {
      if (sha % 2 != 0) Left(InvalidSha())
      else Right(sha)
    }
    private[this] def validarTexto(
                                    texto: String): Either[MensajeError, String] = {
      if (texto.isEmpty) Left(InvalidText())
      else Right(texto)
    }
  }

  object MensajeServices {
    def cifrar(m: Mensaje3[Plano]): Mensaje3[Cifrado] =
      new Mensaje3[Cifrado](m.texto, m.sha)
  }

}
