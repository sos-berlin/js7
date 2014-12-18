package com.sos.scheduler.engine.common.scalautil.xmls

import java.io.StringReader
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource

/** Like a missing 'javax.xml.transform.stream.StringSource'.
 * @author Joacim Zschimmer
 */
object StringSource {
  def apply(o: String): Source = new StreamSource(new StringReader(o))
}
