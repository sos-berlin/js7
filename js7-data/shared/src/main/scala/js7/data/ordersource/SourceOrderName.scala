package js7.data.ordersource

import js7.base.generic.GenericString

/** Name of an external entity in an OrderSource to be mapped to an OrderId. */
final case class SourceOrderName(string: String) extends GenericString

object SourceOrderName extends GenericString.NonEmpty[SourceOrderName]
{
  protected def unchecked(string: String) = new SourceOrderName(string)
}
