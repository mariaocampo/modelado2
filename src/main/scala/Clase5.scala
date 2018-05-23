object Clase5 {

  def sum(a: String, b: String): String = a + b
  def sum(a: Int)(implicit b: Int): Int = a + b

  implicit val i: Int = "4"
  implicit def toStri: Int => String = _.toString
  implicit def toInt: String => Int = java.lang.Integer.valueOf(_)

  println(sum("string",1))
  println(sum(1))

  case class Persona(nombre: String, edad: Int, patrimonio: Int = 10)
  implicit def personaToInt: Persona => Int = _.edad

  trait Sumable[T] {
    def sumar(a: T, b: T): T
    def zero: T
  }

  object SumableOps {
    implicit object IntSumable extends Sumable[Int] {
      def sumar(a: Int, b: Int): Int = a + b
      def zero: Int = 0
    }
    implicit object StringSumable extends Sumable[String] {
      def sumar(a: String, b: String): String = a + b
      def zero: String = ""
    }
    implicit object PersonaSumable extends Sumable[Persona] {
      def sumar(a: Persona, b: Persona): Persona =
        Persona("juridica", 0, a.patrimonio + b.patrimonio)
      override def zero: Persona = Persona("juridica", 0, 0)
    }
  }

  import SumableOps._
  def sum[T](a: T, b: T)(implicit s: Sumable[T]): T =
    s.sumar(a, b)
  def sum2[T: Sumable](a: T, b: T) =
    implicitly[Sumable[T]].sumar(a, b)
  //Use:
  println(s" ${sum2(Persona("Maria", 23, 100), Persona("Juan", 24, 100))}")

}
