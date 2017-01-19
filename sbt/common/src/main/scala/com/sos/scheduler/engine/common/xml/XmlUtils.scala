package com.sos.scheduler.engine.common.xml

import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import java.io._

object XmlUtils {

  def removeXmlProlog(xml: String) =
    if (xml startsWith "<?") xml.replaceFirst("^<[?][xX][mM][lL].+[?][>]\\w*", "") else xml

  def xmlStringToBoolean(o: String) = o match {
    case "true" | "yes" | "1" ⇒ true
    case "false" | "no" | "0" ⇒ false
    case _ ⇒ throw new IllegalArgumentException(s"Boolean value expected instead of '$o'")
  }

  def xmlByteStringToString(byteString: ByteString): String =
    xmlBytesToString(byteString.toArray)

  def xmlBytesToString(bytes: Array[Byte]): String =
    SafeXML.load(new ByteArrayInputStream(bytes)).toString()
}
