package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.{EmptyResponse, Response, StartProcessResponse}
import com.sos.scheduler.engine.common.scalautil.Logger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Executes public Agent command serialized as XML string.
 * @author Joacim Zschimmer
 */
object CommandXmlExecutor {
  private val logger = Logger(getClass)

  def execute(commandString: String)(executeCommand: Command ⇒ Future[Response]): Future[xml.Elem] =
    (Future.successful(CommandXml.parseString(commandString))
      flatMap executeCommand
      map responseToXml
      recover { case throwable ⇒
        logger.debug(throwable.toString, throwable)
        <ERROR text={throwableToString(throwable)}/>
      }
      map { contentElem ⇒ <spooler><answer>{contentElem}</answer></spooler> })

  private def responseToXml(response: Response): xml.Elem = response match {
    case o: StartProcessResponse ⇒ StartProcessXml.responseToXmlElem(o)
    case EmptyResponse ⇒ EmptyResponse.toXmlElem
  }

  private[xmlcommand] def throwableToString(throwable: Throwable): String = throwable match {
    case _: java.util.concurrent.ExecutionException if throwable.getMessage == "Boxed Error"  && throwable.getCause != null ⇒
      // Scala 2.11.5 failing future suppresses string of causing java.lang.Error
      throwable.getCause.toString
    case _ ⇒ throwable.toString
  }
}
