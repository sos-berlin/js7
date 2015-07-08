package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.commands.{StartProcess, StartSeparateProcess, StartThread}
import com.sos.scheduler.engine.agent.data.responses.StartProcessResponse
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.scheduler.engine.common.utils.TcpUtils.parseTcpPort

/**
 * @author Joacim Zschimmer
 */
object StartProcessXml {

  def parseXml(eventReader: ScalaXMLEventReader): StartProcess = {
    import eventReader._
    parseElement(StartProcess.XmlElementName) {
      val controllerOption = (attributeMap.get("ip_address"), attributeMap.getConverted("tcp_port")(parseTcpPort)) match {
        case (Some(ipAddress), Some(port)) ⇒ Some(s"$ipAddress:$port")
        case (None, None) ⇒ None
        case _ ⇒ throw new IllegalArgumentException("Both or none of attributes ipAddress and port should be given")
      }
      attributeMap.get("kind") match {
        case Some("process") ⇒
          attributeMap.ignore("java_options")
          attributeMap.ignore("java_classpath")
          StartThread(controllerAddressOption = controllerOption)
        case None ⇒
          StartSeparateProcess(
            controllerAddressOption = controllerOption,
            javaOptions = attributeMap.getOrElse("java_options", ""),
            javaClasspath = attributeMap.getOrElse("java_classpath", ""))
        case x ⇒ throw new IllegalArgumentException(s"kind=$x")
      }
    }
  }

  def responseToXmlElem(response: StartProcessResponse): xml.Elem = {
    <process
      process_id={response.processId.value.toString}>{
    }</process>
  }
}
