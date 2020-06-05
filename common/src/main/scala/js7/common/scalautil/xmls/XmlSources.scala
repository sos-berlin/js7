package js7.common.scalautil.xmls

import akka.util.ByteString
import java.io.{ByteArrayInputStream, InputStream, StringReader}
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object XmlSources {

  implicit def stringToSource(o: String): Source =
    new StreamSource(new StringReader(o))

  /** Slow */
  implicit def xmlElemToSource(o: xml.Elem): Source =
    new StreamSource(new StringReader(o.toString))

  implicit def inputStreamSource(o: InputStream): StreamSource =
    new StreamSource(o)

  def simpleByteStringSource(o: ByteString): StreamSource =
    new ByteArrayInputStream(o.toArray[Byte])
}
