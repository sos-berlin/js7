package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands.SendProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.ConvertingPF
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader

/**
 * @author Joacim Zschimmer
 */
object SendProcessSignalXml {
  def parseXml(eventReader: ScalaXMLEventReader): SendProcessSignal = {
    import eventReader._
    parseElement(SendProcessSignal.XmlElementName) {
      SendProcessSignal(
        processId = attributeMap.convert("process_id") { o ⇒ AgentProcessId(o.toLong) },
        signal = attributeMap.convert("signal")(ProcessSignal.valueOf))
    }
  }

  def responseXmlElem: xml.Elem = <ok/>
}
