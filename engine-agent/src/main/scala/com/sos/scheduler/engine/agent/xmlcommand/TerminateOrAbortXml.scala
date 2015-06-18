package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.commands.{AbortImmediately, Terminate, TerminateOrAbort}
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import java.time.Duration


/**
 * @author Joacim Zschimmer
 */
object TerminateOrAbortXml {

  def parseXml(eventReader: ScalaXMLEventReader): TerminateOrAbort = {
    import eventReader._
    parseElement(Terminate.XmlElementName) {
      attributeMap("cmd") match {
        case "abort_immediately" ⇒
          AbortImmediately
        case "terminate" ⇒
          attributeMap.getConverted("timeout") { o ⇒ Duration.ofSeconds(o.toLong) } match {
            case Some(timeout) ⇒ Terminate(sigkillProcessesAfter = Some(timeout))
            case None ⇒ Terminate()
          }
      }
    }
  }
}
