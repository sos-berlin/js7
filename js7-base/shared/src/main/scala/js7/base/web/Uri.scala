package js7.base.web

import js7.base.annotation.javaApi
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

  /** Concats with exactly one slash between the parts but returns `this` if `tail` is empty. */
  def /?(tail: String): Uri =
    if (tail.isEmpty) this
    else this / tail
}

object Uri extends GenericString.NonEmpty[Uri]
{
  protected def unchecked(string: String) = new Uri(string)

  @javaApi
  def of(validUri: String) = apply(validUri)
}
