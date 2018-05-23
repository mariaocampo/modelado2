object Clase2 {

  def factorial(x: Int): Int = {
    if (x <= 1) {
      x
    } else
      x * factorial(x - 1)
  }
  def fibonacci(x: Int): Int = {
    if (x > 100)
      x
    else
      fibonacci(x)
  }

  def conditional(b: Boolean, whenTrue: Any, whenFalse: Any): Any = {
    if (b) whenTrue else whenFalse
  }

  def conditional2(b: Boolean, y: Boolean, whenTrue: Any, whenFalse: Any): Any = {
    if (b) whenTrue
    else if (y)
      whenTrue
    else whenFalse
  }

  def whenByName(b: Boolean, whenTrue: Any, whenFalse: Any): Any = {
    if (b) whenTrue else whenFalse
  }
  def whenByValue(b: Boolean, whenTrue: => Any, whenFalse: => Any): Any = {
    if (b) whenTrue else whenFalse
  }

  whenByName(1 == 1, println("si"), println("no")) // Ambos se ejecutan. Los parámetros se evalúan siempre
  whenByValue(1 == 1, println("si"), println("no")) // Los parámatros son como funciones : se evalúan solo cuando se llaman


  def printer(x: Int): String = {
    s"el factorial del número es $x"
  }

  def suma(x: Int, y: Int): Int = x + y

  def hof1[A, B](x: A, msg: String, f: A => B): String = {
    s"el $msg es: ${f(x)}"
  }

  def division(x: Double, y: Double): Double = {
    if (y <= 0) Double.NaN
    else x / y
  }

}

