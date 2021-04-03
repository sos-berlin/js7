package js7.agent.command

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import js7.agent.command.CommandActor._
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachSimpleItem, Batch, CoupleController, DetachItem, EmergencyStop, NoOperation, OrderCommand, RegisterAsController, Response, ShutDown, TakeSnapshot}
import js7.agent.scheduler.AgentHandle
import js7.base.auth.UserId
import js7.base.circeutils.JavaJsonCodecs.instant.StringInstantJsonCodec
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.common.system.startup.Halt
import js7.core.command.{CommandMeta, CommandRegister, CommandRun}
import js7.data.command.{CommandHandlerDetailed, CommandHandlerOverview, InternalCommandId}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}
import scala.util.{Success, Try}

/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
final class CommandActor(agentHandle: AgentHandle)(implicit s: Scheduler)
extends Actor {

  private val register = new CommandRegister[AgentCommand]

  def receive = {
    case Input.Execute(command, meta, response) =>
      executeCommand(command, meta, response)

    case Command.GetOverview =>
      sender() ! register.overview

    case Command.GetDetailed =>
      sender() ! register.detailed

    case Internal.Respond(run, promise, response) =>
      def msg = s"Response to ${run.idString} ${AgentCommand.jsonCodec.classToName(run.command.getClass)} (${run.runningSince.elapsed.pretty}): $response"
      if (run.batchInternalId.isEmpty) {
        if (response == Success(Right(AgentCommand.Response.Accepted))) logger.trace(msg) else logger.debug(msg)
      }
      register.remove(run.internalId)
      promise.complete(response)
  }

  private def executeCommand(command: AgentCommand, meta: CommandMeta, promise: Promise[Checked[Response]], batchId: Option[InternalCommandId] = None): Unit = {
    val run = register.add(meta.user.id, command, batchId)
    logCommand(meta.user.id, run)
    val myResponse = Promise[Checked[Response]]()
    executeCommand2(batchId, run.internalId, command, meta,myResponse)
    myResponse.future onComplete { tried =>
      self ! Internal.Respond(run, promise, tried)
    }
  }

  private def logCommand(userId: UserId, run: CommandRun[AgentCommand]): Unit =
    run.command match {
      case Batch(_) =>  // Log only individual commands
      case _ => logger.debug(run.toString)
    }

  private def executeCommand2(batchId: Option[InternalCommandId], id: InternalCommandId, command: AgentCommand, meta: CommandMeta,
    response: Promise[Checked[Response]]): Unit
  =
    command match {
      case Batch(commands) =>
        val responses = Vector.fill(commands.size) { Promise[Checked[Response]]() }
        for ((c, r) <- commands zip responses)
          executeCommand(c, meta, r, batchId orElse Some(id))
        val singleResponseFutures = responses.map(_.future)
        response.completeWith(
          Future.sequence(singleResponseFutures)
            .map(checkedResponse => Right(AgentCommand.Batch.Response(checkedResponse))))

      case NoOperation =>
        response.success(Right(AgentCommand.Response.Accepted))

      case command @ (_: OrderCommand | _: RegisterAsController | _: CoupleController |
                      _: TakeSnapshot.type | _: AttachSimpleItem | _: DetachItem | _: ShutDown) =>
        // FIXME Delay CoupleController until all AttachOrder (extends OrderCommand) (and DetachOrder?) have been finished, to return a properly updated state
        agentHandle.executeCommand(command, meta.user.id, response)

      case EmergencyStop(restart) =>
        Halt.haltJava("Command EmergencyStop received: JS7 AGENT STOPS NOW", restart = restart)
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
    final case class Execute(command: AgentCommand, meta: CommandMeta, response: Promise[Checked[Response]])
  }

  private object Internal {
    final case class Respond(run: CommandRun[AgentCommand], promise: Promise[Checked[Response]], response: Try[Checked[Response]])
  }

  final class Handle(actor: ActorRef)(implicit askTimeout: Timeout) extends CommandHandler {
    def execute(command: AgentCommand, meta: CommandMeta) =
      Task.deferFuture {
        val promise = Promise[Checked[Response]]()
        actor ! Input.Execute(command, meta, promise)
        promise.future
      }

    def overview =
      Task.deferFuture(
        (actor ? Command.GetOverview).mapTo[CommandHandlerOverview])

    def detailed =
      Task.deferFuture(
        (actor ? Command.GetDetailed).mapTo[CommandHandlerDetailed[AgentCommand]])
  }
}
