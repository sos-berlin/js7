package js7.base.web

import java.net.{URI, URISyntaxException}
import js7.base.annotation.javaApi
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.base.web.Uri.StripPathRegex

final case class Uri(string: String) extends GenericString:

  private lazy val javaUri: Checked[URI] =
    try Right(new URI(string))
    catch { case t: URISyntaxException =>
      Left(Problem.pure("Invalid URI: " + t.getMessage))
    }

  def port: Checked[Int] =
    javaUri.flatMap(_.getPort match {
      case -1 => Left(Problem.pure(s"URI has no port number: $string"))
      case n => Right(n)
    })

  /** Concats with exactly one slash between the parts. */
  def /(tail: String): Uri =
    if string.endsWith("/") != tail.startsWith("/") then
      Uri(string + tail)
    else if string.endsWith("/") && tail.startsWith("/") then
      Uri(string + tail.tail)
    else
      Uri(string + '/' + tail)

  /** Concats with exactly one slash between the parts but returns `this` if `tail` is empty. */
  def /?(tail: String): Uri =
    if tail.isEmpty then this
    else this / tail

  def stripPath: Uri =
    string match
      case StripPathRegex(result) => if result == string then this else Uri(result)
      case _ => this


object Uri extends GenericString.NonEmpty[Uri]:
  private val StripPathRegex = "([^/]*//[^/]+/).*".r

  protected def unchecked(string: String) = 
    new Uri(string)

  @javaApi
  def of(validUri: String): Uri = 
    apply(validUri)
