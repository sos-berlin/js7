package js7.base.time

import js7.base.generic.GenericString

final case class TimeZone(string: String) extends GenericString

object TimeZone extends GenericString.NonEmpty[TimeZone]
{
  val utc = new TimeZone("UTC")

  protected def unchecked(string: String) = new TimeZone(string)
}
