package js7.base.web

import js7.base.generic.GenericString

final case class Uri(string: String) extends GenericString

object Uri extends GenericString.NonEmpty[Uri]
{
  def unchecked(string: String) = new Uri(string)
}
