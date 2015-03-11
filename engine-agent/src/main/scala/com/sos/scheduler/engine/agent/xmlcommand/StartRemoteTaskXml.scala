package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.commands.{StartRemoteTask, StartRemoteTaskResponse}
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import java.net.{InetAddress, InetSocketAddress}

/**
 * @author Joacim Zschimmer
 */
object StartRemoteTaskXml {
  def parseXml(inetAddress: InetAddress, eventReader: ScalaXMLEventReader): StartRemoteTask = {
    import eventReader._
    parseElement() {
      val usesApi = attributeMap.get("kind") match {
        case Some("process") ⇒ false
        case None ⇒ true
        case x ⇒ throw new IllegalArgumentException(s"kind=$x")
      }
      StartRemoteTask(
        controllerAddress = new InetSocketAddress(inetAddress, attributeMap.convert("tcp_port")(stringToTcpPort)),
        usesApi = usesApi,
        javaOptions = attributeMap.getOrElse("java_options", ""),
        javaClassPath = attributeMap.getOrElse("java_classpath", ""))
    }
  }

  private def stringToTcpPort(string: String) =
    try {
      val result = string.toInt
      require(result > 1 && result <= 0xffff)
      result
    }
    catch { case e: Exception ⇒ throw new IllegalArgumentException(s"Invalid TCP Port: $string") }

  def responseToXmlElem(response: StartRemoteTaskResponse): xml.Elem =
      <process process_id={response.remoteTaskId.value.toString}/>
}
