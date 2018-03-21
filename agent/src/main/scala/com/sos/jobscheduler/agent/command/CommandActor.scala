package com.sos.jobscheduler.agent.command

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.tagging.@@
import com.sos.jobscheduler.agent.command.CommandActor._
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.instant.StringInstantJsonCodec
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import io.circe.generic.JsonCodec
import java.time.Instant
import java.time.Instant.now
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Success, Try}

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
      if (run.batchInternalId.isEmpty || response != Success(Batch.Succeeded(Accepted))) {
        logger.debug(s"Response to ${run.idString} ${run.command.getClass.getSimpleName stripSuffix "$"} (${(now - run.startedAt).pretty}): $response")
      }
      idToCommand -= run.internalId
      promise.complete(response)
  }

  def executeCommand(command: AgentCommand, meta: CommandMeta, promise: Promise[Response], batchId: Option[InternalCommandId] = None): Unit = {
    totalCounter += 1
    val id = idGenerator.next()
    val run = CommandRun(id, batchId, now, command)
    logCommand(run)
    idToCommand += id → run
    val myResponse = Promise[Response]()
    executeCommand2(batchId, id, command, meta, myResponse)
    myResponse.future onComplete { tried ⇒
      self ! Internal.Respond(run, promise, tried)
    }
  }

  private def logCommand(run: CommandRun): Unit =
    run.command match {
      case Batch(_) ⇒ // Log only individual commands
      case _ ⇒ logger.info(run.toString)
        //if (run.command.toStringIsLonger) logger.debug(s"${run.idString} ${run.command}")  // Complete string
    }

  private def executeCommand2(batchId: Option[InternalCommandId], id: InternalCommandId, command: AgentCommand, meta: CommandMeta, response: Promise[Response]): Unit = {
    command match {
      case Batch(commands) ⇒
        val responses = Vector.fill(commands.size) { Promise[Response] }
        for ((c, r) ← commands zip responses)
          executeCommand(c, meta, r, batchId orElse Some(id))
        val singleResponseFutures = responses map { _.future } map { _
          .map(Batch.Succeeded.apply)
          .recover { case t ⇒ Batch.Failed(t.toString) }}
        response.completeWith(Future.sequence(singleResponseFutures) map Batch.Response.apply)

      case command: SessionCommand ⇒
        sessionActor.forward(SessionActor.Command.Execute(command, meta, response))

      case NoOperation ⇒
        response.success(AgentCommand.Accepted)

      case command @ (_: OrderCommand | _: RegisterAsMaster.type | _: Terminate) ⇒
        agentHandle.executeCommand(command, meta.user.id, response)

      case EmergencyStop ⇒
        val msg = "Command EmergencyStop received: JOBSCHEDULER AGENT STOPS NOW"
        logger.error(msg)
        Log4j.shutdown()
        sys.runtime.halt(99)
    }
  }
}

object CommandActor {
  intelliJuseImport(StringInstantJsonCodec)

  private val logger = Logger(getClass)

  object Command {
    final case object GetOverview
    final case object GetDetailed
  }

  object Input {
    final case class Execute(command: AgentCommand, meta: CommandMeta, response: Promise[Response])
  }

  private object Internal {
    final case class Respond(run: CommandRun, promise: Promise[Response], response: Try[Response])
  }

  @JsonCodec
  final case class CommandRun(internalId: InternalCommandId, batchInternalId: Option[InternalCommandId], startedAt: Instant, command: AgentCommand) {

    override def toString = s"$idString ${command.toShortString}"

    def idString = batchInternalId match {
      case None ⇒ internalId.toString  // #100
      case Some(batchId) ⇒ s"$batchId+${internalId.number - batchId.number}"  // #100+1
    }

    def overview = new CommandRunOverview(internalId, startedAt, command)
  }

  final class Handle(actor: ActorRef)(implicit askTimeout: Timeout) extends CommandHandler {
    def execute(command: AgentCommand, meta: CommandMeta) = {
      val promise = Promise[Response]()
      actor ! Input.Execute(command, meta, promise)
      promise.future
    }

    def overview = (actor ? Command.GetOverview).mapTo[CommandHandlerOverview]

    def detailed = (actor ? Command.GetDetailed).mapTo[CommandHandlerDetailed]
  }
}
