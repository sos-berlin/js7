package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.utils.TcpUtils.parseTcpPort
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments._
import java.net.InetSocketAddress
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskStartArguments(controllerAddress: String) {
  def controllerInetSocketAddress = toInetSocketAddress(controllerAddress)
}

object TaskStartArguments {

  private val HostPortRegex = "(.*):(\\d+)".r

  private[task] def toInetSocketAddress(string: String): InetSocketAddress =
    string match {
      case HostPortRegex(host, port) ⇒ new InetSocketAddress(host, parseTcpPort(port))
    }

  implicit val MyJsonFormat = jsonFormat1(apply)
}
