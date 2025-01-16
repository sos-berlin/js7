package js7.data.orderwatch

import js7.base.generic.GenericString

/** Name of an external entity in an OrderWatch to be mapped to an OrderId. */
final case class ExternalOrderName(string: String) extends GenericString:
  override def toString = s"ExternalOrderName($string)"


object ExternalOrderName extends GenericString.NonEmpty[ExternalOrderName]:
  protected def unchecked(string: String) = new ExternalOrderName(string)
