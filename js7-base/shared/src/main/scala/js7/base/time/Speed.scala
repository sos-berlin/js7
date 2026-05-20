package js7.base.time

import js7.base.time.ScalaTime.*
import scala.concurrent.duration.FiniteDuration

/** Speed expressed as weight per period. */
final case class Speed(weight: Double, period: FiniteDuration, unit: SpeedUnit = SpeedUnit.empty):

  override lazy val toString =
    val sb = new StringBuilder
    sb.append(weight.toString.stripSuffix(".0"))
    if weight == 1.0 then
      if unit.singular.nonEmpty then
        sb.append(' ').append(unit.singular)
    else
      if unit.plural.nonEmpty then
        sb.append(' ').append(unit.plural)
    sb.append('/')
    sb.append(period.show)
    sb.toString()


final case class SpeedUnit(singular: String, plural: String):
  def isEmpty: Boolean =
    singular.isEmpty && plural.isEmpty

  override def equals(o: Any) =
    o match
      case o: SpeedUnit => isEmpty || o.isEmpty || singular == o.singular && plural == o.plural
      case _ => false

object SpeedUnit:
  val empty = SpeedUnit("", "")
