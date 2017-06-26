package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.agent.command.AgentCommandHandler._
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commandresponses.{EmptyResponse, LoginResponse}
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.data.commands._
import com.sos.jobscheduler.agent.fileordersource.{FileCommandExecutor, RequestFileOrderSourceContentExecutor}
import com.sos.jobscheduler.agent.scheduler.OrderHandler
import com.sos.jobscheduler.agent.task.TaskHandler
import com.sos.jobscheduler.agent.web.common.LoginSession
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.common.auth.User
import com.sos.jobscheduler.common.scalautil.{Logger, ScalaConcurrentHashMap}
import com.sos.jobscheduler.common.sprayutils.web.session.SessionRegister
import com.sos.jobscheduler.data.session.SessionToken
import java.time.Instant
import java.time.Instant.now
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import spray.json.DefaultJsonProtocol._

/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentCommandHandler @Inject private(
  sessionRegister: SessionRegister[LoginSession],
  taskHandler: TaskHandler,
  orderHandler: OrderHandler,
  executeRequestFileOrderSourceContent: RequestFileOrderSourceContentExecutor,
  agentConfiguration: AgentConfiguration)
  (implicit ec: ExecutionContext)
extends CommandExecutor
with CommandHandlerOverview
with CommandHandlerDetailed {

  private val nextId = new AtomicLong(0)
  private val totalCounter = new AtomicInteger(0)
  private val idToCommand = new ScalaConcurrentHashMap[InternalCommandId, CommandRun]

  def totalCommandCount = totalCounter.get
  def currentCommandCount = idToCommand.size
  def commandRuns = (idToCommand.values map { _.overview }).toVector

  def executeCommand(command: AgentCommand, meta: CommandMeta): Future[command.Response] = {
    totalCounter.incrementAndGet()
    val id = InternalCommandId(nextId.incrementAndGet())
    logger.info(s"$id ${command.toShortString}")
    if (command.toStringIsLonger) logger.debug(s"$id $command")  // Complete string
    idToCommand += id → CommandRun(id, now(), command)
    val future = executeCommand2(id, command, meta)
    future onComplete { _ ⇒ idToCommand -= id }
    future
  }

  private def executeCommand2(id: InternalCommandId, command: AgentCommand, meta: CommandMeta) =
    (command match {
      case command: FileCommand ⇒ Future.successful(FileCommandExecutor.executeCommand(command))
      case Login ⇒ login(meta.sessionTokenOption, meta.user)
      case Logout ⇒ logout(meta.sessionTokenOption)
      case NoOperation ⇒ Future.successful(EmptyResponse)
      case command: TaskCommand ⇒ taskHandler.execute(command, meta)
      case command: TerminateOrAbort ⇒ terminateOrAbort(command, meta)
      case command: RequestFileOrderSourceContent ⇒ executeRequestFileOrderSourceContent(command)
      case command @ (_: OrderCommand | _: RegisterAsMaster.type) ⇒ orderHandler.execute(meta.user.id, command)
    }) map { response ⇒
      logger.debug(s"Response to $id ${command.getClass.getSimpleName}: $response")
      response.asInstanceOf[command.Response]
    }

  private def login(currentSessionTokenOption: Option[SessionToken], user: User): Future[LoginResponse] =
    Future fromTry Try {
      currentSessionTokenOption foreach sessionRegister.remove
      LoginResponse(sessionRegister.add(LoginSession(user)))
    }

  private def logout(sessionTokenOption: Option[SessionToken]): Future[EmptyResponse.type] =
    Future fromTry Try {
      val sessionToken = sessionTokenOption getOrElse {
        throw new IllegalArgumentException("Logout without SessionToken")
      }
      sessionRegister.remove(sessionToken)
      EmptyResponse
    }

  private def terminateOrAbort(command: TerminateOrAbort, meta: CommandMeta): Future[command.Response] = {
    val taskHandlerTerminated = taskHandler.execute(command, meta)
    val orderHandlerTerminated = orderHandler.execute(meta.user.id, command)
    Future.sequence(List(taskHandlerTerminated, orderHandlerTerminated))
      .map { _ ⇒ EmptyResponse.asInstanceOf[command.Response] }
  }
}

object AgentCommandHandler {
  private val logger = Logger(getClass)

  final case class CommandRun(internalId: InternalCommandId, startedAt: Instant, command: AgentCommand) {
    def overview = new CommandRunOverview(internalId, startedAt, command)
  }

  object CommandRun {
    implicit def MyJsonFormat = jsonFormat3(apply)
  }
}
