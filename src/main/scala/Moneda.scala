import com.sun.javafx.collections.MappingChange.Map
import com.sun.tools.javac.util.List

object Moneda {

  sealed trait Currency(val v: Double)
  case class USD(value: Double) extends Currency
  case class EUR(value: Double) extends Currency
  case class COP(value: Double) extends Currency

  object Divisas {
    def toCOP(c: Currency,
              trm: List[(Currency, Double)],
              func: Double => List[(Currency, Double)] => Double): Double = {
      c match {
        case usd @ USD(v) => func(v)(trm)
        case eur @ EUR(v) => func(v)(trm)
        case cop @ COP(v) => func(v)(trm)
      }
    }

    def trms(currency: Currency)(v: Double)(trm: List[(Currency, Double)]): Double = {
      v * trm.find {
          case (usd, trm) => true
        }.get._2
    }
  }

}
