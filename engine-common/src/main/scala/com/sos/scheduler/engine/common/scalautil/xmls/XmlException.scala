package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.base.exceptions.PublicException
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.RichAny
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader.locationToString
import javax.xml.stream.Location
import scala.annotation.tailrec
import scala.language.implicitConversions

/**
 * @author Joacim Zschimmer
 */
final class XmlException(elementName: String, location: Location, cause: Exception = null, message: String = "")
  extends RuntimeException(cause) with PublicException {

  override def toString = s"XmlException: $getMessage"
  override def getMessage = s"$nonWrappedCauseString - In $text"
  def publicMessage = {
    val locationString = Option(location) map { o ⇒ s" (line ${o.getLineNumber}:${o.getColumnNumber})" } getOrElse ""
    s"Error in XML element <$elementName>$locationString: ${message.substitute("" → "ERROR")}"
  }

  private def nonWrappedCauseString =
    Option(nonWrappedCause) map { _.toString stripPrefix "java.lang.RuntimeException: " } getOrElse s"Error in XML element <$elementName>"

  @tailrec
  def nonWrappedCause: Throwable = getCause match {
    case cause: XmlException ⇒ cause.nonWrappedCause
    case o ⇒ o
  }

  private def text: String =
    s"<$elementName> (${locationToString(location)})" + (
      getCause match {
        case cause: XmlException ⇒ " " + cause.text
        case _ ⇒ ""
      })
}

object XmlException {
  def unapply(o: XmlException) = o.getCause match {
    case cause: XmlException ⇒ Some(cause.nonWrappedCause)
    case cause ⇒ Some(cause)
  }
}

