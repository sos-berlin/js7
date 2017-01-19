package com.sos.scheduler.engine.common.scalautil.xmls

import java.io.{InputStream, StringReader}
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
}
