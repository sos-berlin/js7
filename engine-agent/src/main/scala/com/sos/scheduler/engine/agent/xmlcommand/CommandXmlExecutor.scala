package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.commands.{CloseProcessResponse, Command, Response, StartProcessResponse}
import com.sos.scheduler.engine.agent.xmlcommand.CommandXmlExecutor._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Executes public Agent command serialized as XML string.
 * @author Joacim Zschimmer
 */
final class CommandXmlExecutor(executeCommand: Command ⇒ Future[Response]) {

  def execute(commandString: String): Future[xml.Elem] =
    (Future { CommandXml.parseString(commandString) }
      flatMap executeCommand
      map responseToXml
      recover { case throwable ⇒ <ERROR text={throwableToString(throwable)}/> }
      map { contentElem ⇒ <spooler><answer>{contentElem}</answer></spooler> })

  private def responseToXml(response: Response): xml.Elem = response match {
    case o: StartProcessResponse ⇒ StartProcessXml.responseToXmlElem(o)
    case CloseProcessResponse ⇒ CloseProcessXml.responseXmlElem
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
