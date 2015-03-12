package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.commands.{CloseRemoteTaskResponse, Command, Response, StartRemoteTaskResponse}
import com.sos.scheduler.engine.agent.xmlcommand.CommandXmlExecutor._
import java.net.InetAddress
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Executes public Agent command serialized as XML string.
 * @author Joacim Zschimmer
 */
final class CommandXmlExecutor(executeCommand: Command ⇒ Future[Response]) {

  def execute(clientIPAddress: InetAddress, commandString: String): Future[xml.Elem] =
    (Future { CommandXml.parseString(clientIPAddress, commandString) }
      flatMap executeCommand
      map responseToXml
      recover { case throwable ⇒ <ERROR text={throwableToString(throwable)}/> }
      map { contentElem ⇒ <spooler><answer>{contentElem}</answer></spooler> })

  private def responseToXml(response: Response): xml.Elem = response match {
    case o: StartRemoteTaskResponse ⇒ StartRemoteTaskXml.responseToXmlElem(o)
    case CloseRemoteTaskResponse ⇒ CloseRemoteTaskXml.responseXmlElem
  }
}

object CommandXmlExecutor {
  private[xmlcommand] def throwableToString(throwable: Throwable): String = throwable match {
    case _: java.util.concurrent.ExecutionException if throwable.getMessage == "Boxed Error"  && throwable.getCause != null ⇒
      // Scala 2.11.5 failing future suppresses string of causing java.lang.Error
      throwable.getCause.toString
    case _ ⇒ throwable.toString
  }
}
