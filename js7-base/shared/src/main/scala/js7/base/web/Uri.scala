package js7.base.web

import js7.base.generic.GenericString

final case class Uri(string: String) extends GenericString
{
  /** Concats with exactly one slash between the parts. */
  def /(tail: String): Uri =
    if (string.endsWith("/") != tail.startsWith("/"))
      Uri(string + tail)
    else if (string.endsWith("/") && tail.startsWith("/"))
      Uri(string + tail.tail)
    else
      Uri(string + '/' + tail)
}

object Uri extends GenericString.NonEmpty[Uri]
{
  def unchecked(string: String) = new Uri(string)
}
