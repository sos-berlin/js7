package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.commands.Terminate
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import java.time.Duration


/**
 * @author Joacim Zschimmer
 */
object TerminateXml {

  def parseXml(eventReader: ScalaXMLEventReader): Terminate = {
    import eventReader._
    parseElement(Terminate.XmlElementName) {
      attributeMap("cmd") match {
        case "abort_immediately" ⇒
          Terminate(sigkillProcessesAfter = Duration.ZERO)
        case "terminate" ⇒
          attributeMap.getConverted("timeout") { o ⇒ Duration.ofSeconds(o.toLong) } match {
            case Some(timeout) ⇒ Terminate(sigkillProcessesAfter = timeout)
            case None ⇒ Terminate()
          }
      }
    }
  }
}
