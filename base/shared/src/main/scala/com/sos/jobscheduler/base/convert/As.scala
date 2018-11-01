package com.sos.jobscheduler.base.convert

import com.sos.jobscheduler.base.utils.DecimalPrefixes

/**
  * @author Joacim Zschimmer
  */
trait As[V, W] {
  // Objects of this class are implicitly used. So do not extend V ⇒ W to avoid implicit use as function.
  def apply(v: V): W
}

object As {

  def apply[V, W](asW: V ⇒ W): As[V, W] =
    v ⇒ asW(v)

  def convert[V, W](from: V)(implicit to: As[V, W]): W =
    to(from)

  implicit val StringAsString: As[String, String] = As(identity)

  implicit object StringAsBoolean extends As[String, Boolean] {
    val StringToBooleanMap = Map(
      "true"  → true , "on"  → true , "yes" → true,
      "false" → false, "off" → false, "no"  → false)

    def apply(o: String) = StringToBooleanMap.getOrElse(o,
      throw new IllegalArgumentException(s"Boolean value true or false expected, not: $o"))
  }

  //
  // Some default implementations
  //

  implicit val StringAsInt: As[String, Int] =
    As(Integer.parseInt)

  implicit val StringAsLong: As[String, Long] =
    As(java.lang.Long.parseLong)

  implicit val StringAsBigDecimal: As[String, BigDecimal] =
    As(o ⇒ new java.math.BigDecimal(o))

  val StringAsLongWithDecimalPrefix: As[String, Long] =
    As(DecimalPrefixes.toInt)

  val StringAsByteCountWithDecimalPrefix: As[String, Long] =
    As(o ⇒ DecimalPrefixes.toInt(o stripSuffix "B"/*optional*/))
}
