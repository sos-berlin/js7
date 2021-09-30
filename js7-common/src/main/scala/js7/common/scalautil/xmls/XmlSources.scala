package js7.common.scalautil.xmls

import java.io.{InputStream, StringReader}
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import js7.base.data.ByteArray
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object XmlSources {

  implicit def stringToSource(o: String): Source =
    new StreamSource(new StringReader(o))

  implicit def inputStreamSource(o: InputStream): StreamSource =
    new StreamSource(o)

  def simpleByteArraySource(o: ByteArray): StreamSource =
    o.toInputStream
}
