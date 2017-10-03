package com.sos.jobscheduler.agent.command

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.tagging.@@
import com.sos.jobscheduler.agent.command.CommandActor._
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.Instant
import java.time.Instant.now
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
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
  private val idToCommand = mutable.Map[InternalCommandId, CommandRun]()
  private val idGenerator = InternalCommandId.newGenerator()

  def receive = {
    case Input.Execute(command, meta, response) ⇒
      executeCommand(command, meta, response)

    case Command.GetOverview ⇒
      sender() ! CommandHandlerOverview(currentCommandCount = idToCommand.size, totalCommandCount = totalCounter)

    case Command.GetDetailed ⇒
      sender() ! CommandHandlerDetailed((idToCommand.values map { _.overview }).toVector)

    case Internal.Respond(run, promise, response) ⇒
      logger.debug(s"Response to ${run.idString} ${run.command.getClass.getSimpleName} (${(now - run.startedAt).pretty}): $response")
      idToCommand -= run.internalId
      promise.complete(response)
  }

  def executeCommand(command: AgentCommand, meta: CommandMeta, promise: Promise[AgentCommand.Response], compoundId: Option[InternalCommandId] = None): Unit = {
    totalCounter += 1
    val id = idGenerator.next()
    val run = CommandRun(id, compoundId, now, command)
    logger.info(run.toString)
    if (command.toStringIsLonger) logger.debug(s"${run.idString} $command")  // Complete string
    idToCommand += id → run
    val myResponse = Promise[AgentCommand.Response]()
    executeCommand2(compoundId, id, command, meta, myResponse)
    myResponse.future onComplete { tried ⇒
      self ! Internal.Respond(run, promise, tried)
    }
  }

  private def executeCommand2(compoundId: Option[InternalCommandId], id: InternalCommandId, command: AgentCommand, meta: CommandMeta, response: Promise[AgentCommand.Response]): Unit = {
    import AgentCommand._
    command match {
      case Compound(commands) ⇒
        val responses = Vector.fill(commands.size) { Promise[AgentCommand.Response] }
        for ((c, r) ← commands zip responses)
          executeCommand(c, meta, r, compoundId orElse Some(id))
        val singleResponseFutures = responses map { _.future } map { _
          .map(Compound.Succeeded.apply)
          .recover { case t ⇒ Compound.Failed(t.toString) }}
        response.completeWith(Future.sequence(singleResponseFutures) map Compound.Response.apply)

      case command: SessionCommand ⇒
        sessionActor.forward(SessionActor.Command.Execute(command, meta, response))

      case NoOperation ⇒
        response.success(AgentCommand.Accepted)

      case command @ (_: OrderCommand | _: RegisterAsMaster.type | _: Terminate) ⇒
        agentHandle.executeCommand(command, meta.user.id, response)

      case AbortImmediately ⇒
        val msg = "Command AbortImmediately received: JOBSCHEDULER AGENTS ABORTS NOW"
        logger.error(msg)
        Log4j.shutdown()
        sys.runtime.halt(99)
    }
  }
}

object CommandActor {
  private val logger = Logger(getClass)

  object Command {
    final case object GetOverview
    final case object GetDetailed
  }

  object Input {
    final case class Execute(command: AgentCommand, meta: CommandMeta, response: Promise[AgentCommand.Response])
  }

  private object Internal {
    final case class Respond(run: CommandRun, promise: Promise[AgentCommand.Response], response: Try[AgentCommand.Response])
  }

  final case class CommandRun(internalId: InternalCommandId, compoundInternalId: Option[InternalCommandId], startedAt: Instant, command: AgentCommand) {

    override def toString = s"$idString ${command.toShortString}"

    def idString = (compoundInternalId map { o ⇒ s"$o." } getOrElse "") + s"$internalId "

    def overview = new CommandRunOverview(internalId, startedAt, command)
  }

  object CommandRun {
    implicit def MyJsonFormat = jsonFormat4(apply)
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
