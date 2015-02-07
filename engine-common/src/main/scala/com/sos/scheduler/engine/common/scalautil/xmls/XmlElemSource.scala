package com.sos.scheduler.engine.common.scalautil.xmls

import java.io.StringReader
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource

/**
 * @author Joacim Zschimmer
 */
object XmlElemSource {
  def apply(o: xml.Elem): Source = new StreamSource(new StringReader(o.toString))
}
