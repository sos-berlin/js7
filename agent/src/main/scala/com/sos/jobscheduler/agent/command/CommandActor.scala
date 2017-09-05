package com.sos.jobscheduler.agent.command

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.tagging.@@
import com.sos.jobscheduler.agent.command.CommandActor._
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.data.commands._
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import java.time.Instant
import java.time.Instant.now
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.Try
import spray.json.DefaultJsonProtocol._

/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
final class CommandActor(sessionActor: ActorRef @@ SessionActor, agentHandle: AgentHandle)(implicit ec: ExecutionContext)
extends Actor {

  private var totalCounter = 0L
  private var nextId = 0L
  private val idToCommand = mutable.Map[InternalCommandId, CommandRun]()


  def receive = {
    case Input.Execute(command, meta, response) ⇒
      executeCommand(command, meta, response)

    case Command.GetOverview ⇒
      sender() ! CommandHandlerOverview(currentCommandCount = idToCommand.size, totalCommandCount = totalCounter)

    case Command.GetDetailed ⇒
      sender() ! CommandHandlerDetailed((idToCommand.values map { _.overview }).toVector)

    case Internal.Respond(id, command, promise, response) ⇒
      logger.debug(s"Response to $id ${command.getClass.getSimpleName}: $response")
      idToCommand -= id
      promise.complete(response)
  }

  def executeCommand(command: AgentCommand, meta: CommandMeta, promise: Promise[Response]) = {
    totalCounter += 1
    nextId += 1
    val id = InternalCommandId(nextId)
    logger.info(s"$id ${command.toShortString}")
    if (command.toStringIsLonger) logger.debug(s"$id $command")  // Complete string
    idToCommand += id → CommandRun(id, now, command)
    val myResponse = Promise[Response]()
    executeCommand2(id, command, meta, myResponse)
    myResponse.future onComplete { tried ⇒
      self ! Internal.Respond(id, command, promise, tried)
    }
  }

  private def executeCommand2(id: InternalCommandId, command: AgentCommand, meta: CommandMeta, response: Promise[Response]) =
    command match {
      case command: SessionCommand ⇒
        sessionActor.forward(SessionActor.Command.Execute(command, meta, response))

      case NoOperation ⇒
        response.success(EmptyResponse)

      case command @ (_: OrderCommand | _: RegisterAsMaster.type | _: TerminateOrAbort) ⇒
        agentHandle.executeCommand(command, meta.user.id, response)
    }
}

object CommandActor {
  private val logger = Logger(getClass)

  object Command {
    final case object GetOverview
    final case object GetDetailed
  }

  object Input {
    final case class Execute(command: AgentCommand, meta: CommandMeta, response: Promise[Response])
  }

  private object Internal {
    final case class Respond(id: InternalCommandId, command: AgentCommand, promise: Promise[Response], response: Try[Response])
  }

  final case class CommandRun(internalId: InternalCommandId, startedAt: Instant, command: AgentCommand) {
    def overview = new CommandRunOverview(internalId, startedAt, command)
  }

  object CommandRun {
    implicit def MyJsonFormat = jsonFormat3(apply)
  }

  final class Handle(actor: ActorRef)(implicit askTimeout: Timeout) extends CommandHandler {
    def execute(command: AgentCommand, meta: CommandMeta) = {
      val promise = Promise[AgentCommand.Response]()
      actor ! Input.Execute(command, meta, promise)
      promise.future
    }

    def overview = (actor ? Command.GetOverview).mapTo[CommandHandlerOverview]

    def detailed = (actor ? Command.GetDetailed).mapTo[CommandHandlerDetailed]
  }
}
