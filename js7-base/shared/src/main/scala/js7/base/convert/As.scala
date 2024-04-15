package js7.base.convert

import js7.base.problem.Checked.Ops
import js7.base.utils.DecimalPrefixes

/**
  * @author Joacim Zschimmer
  */
trait As[V, W]:
  // Objects of this class are implicitly used. So do not extend V => W to avoid implicit use as function.
  def apply(v: V): W


object As:

  def apply[V, W](asW: V => W): As[V, W] =
    v => asW(v)

  def convert[V, W](from: V)(implicit to: As[V, W]): W =
    to(from)

  implicit val StringAsString: As[String, String] = As(identity)

  implicit object StringAsBoolean extends As[String, Boolean]:
    val StringToBooleanMap: Map[String, Boolean] =
      Map(
        "true"  -> true , "on"  -> true , "yes" -> true,
        "false" -> false, "off" -> false, "no"  -> false)

    def apply(o: String): Boolean =
      StringToBooleanMap.getOrElse(o,
        throw new IllegalArgumentException(s"Boolean value true or false expected, not: $o"))

  //
  // Some default implementations
  //

  implicit val StringAsInt: As[String, Int] =
    As(Integer.parseInt)

  val StringAsIntOrUnlimited: As[String, Option[Int]] =
    As:
      case "unlimited" => None
      case o => Some(Integer.parseInt(o))

  implicit val StringAsLong: As[String, Long] =
    As(java.lang.Long.parseLong)

  implicit val StringAsBigDecimal: As[String, BigDecimal] =
    As(o => new java.math.BigDecimal(o))

  val StringAsLongWithDecimalPrefix: As[String, Long] =
    As(o => DecimalPrefixes.toLong(o).orThrow)

  val StringAsByteCountWithDecimalPrefix: As[String, Long] =
    As(o => DecimalPrefixes.toLong(o stripSuffix "B"/*optional*/).orThrow)

  val StringAsPercentage: As[String, BigDecimal] = As:
    case o if o endsWith "%" => BigDecimal(o.dropRight(1)) / 100
    case o => BigDecimal(o)
