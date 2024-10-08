package js7.agent.command

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.agent.command.CommandActor.*
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachSignedItem, Batch, ClusterSwitchOver, CoupleController, DedicateAgentDirector, DetachItem, EmergencyStop, NoOperation, OrderCommand, Reset, ResetSubagent, Response, ShutDown, TakeSnapshot}
import js7.agent.scheduler.AgentHandle
import js7.base.catsutils.CatsEffectExtensions.catchAsChecked
import js7.base.circeutils.JavaDataJsonCodecs.instant.StringInstantJsonCodec
import js7.base.log.{CorrelId, CorrelIdWrapped, Logger}
import js7.base.monixlike.MonixLikeExtensions.deferFuture
import js7.base.problem.Checked
import js7.base.system.startup.Halt
import js7.base.time.ScalaTime.*
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.core.command.{CommandMeta, CommandRegister, CommandRun}
import org.apache.pekko.actor.{Actor, ActorRef}
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.{Success, Try}

/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
final class CommandActor(agentHandle: AgentHandle)(using ioRuntime: IORuntime)
extends Actor:

  private given ExecutionContext = ioRuntime.compute

  // TODO Don't use CorrelId as command ID in the register
  //  because CorrelId may be used for several commands!
  //  The register is used for inspection via web service only
  private val register = new CommandRegister[AgentCommand]

  def receive: Receive =
    case Input.Execute(command, meta, correlId, response) =>
      correlId.bind:
        executeCommand(command, meta, response)

    case Internal.Respond(run, correlId, promise, response) =>
      correlId.bind[Unit]:
        def msg = s"Response to ${run.idString} " +
          s"${AgentCommand.jsonCodec.classToName(run.command.getClass)} " +
          s"(${run.runningSince.elapsed.pretty}): $response"
        if run.batchInternalId.isEmpty then
          if response == Success(Right(AgentCommand.Response.Accepted)) then
            logger.trace(msg)
          else
            logger.debug(msg)
        register.remove(run.correlId)
        promise.complete(response)

  private def executeCommand(
    command: AgentCommand,
    meta: CommandMeta,
    promise: Promise[Checked[Response]],
    batchId: Option[CorrelId] = None)
  : Unit =
    val run = register.add(command, meta, CorrelId.current, batchId)
    logCommand(run)
    val myResponse = Promise[Checked[Response]]()
    executeCommand2(batchId, run.correlId, command, meta, myResponse)
    myResponse.future onComplete { tried =>
      self ! Internal.Respond(run, run.correlId, promise, tried)
    }

  private def logCommand(run: CommandRun[AgentCommand]): Unit =
    run.command match
      case Batch(_) =>  // Log only individual commands
      case _ => logger.debug(run.toString)

  private def executeCommand2(
    batchId: Option[CorrelId],
    correlId: CorrelId,
    command: AgentCommand,
    meta: CommandMeta,
    response: Promise[Checked[Response]])
  : Unit =
    command match
      case Batch(wrappedCommands) =>
        // Execute one command after the other
        fs2.Stream.iterable(wrappedCommands)
          .evalMap: wrapped =>
            IO.fromFuture(IO:
              val CorrelIdWrapped(subcorrelId, cmd) = wrapped
              val promise = Promise[Checked[Response]]
              subcorrelId.orNew.bind:
                executeCommand(cmd, meta, promise, batchId = batchId orElse Some(correlId))
              promise.future)
          .compile
          .toVector
          .map(AgentCommand.Batch.Response(_))
          .catchAsChecked
          .map:
            response.success
          .unsafeRunAndForget()

      case NoOperation =>
        response.success(Right(AgentCommand.Response.Accepted))

      case command @ (_: OrderCommand | _: DedicateAgentDirector | _: CoupleController | _: Reset |
                      _: TakeSnapshot.type | _: ShutDown |
                      _: AttachItem | _: AttachSignedItem | _: DetachItem | _: ResetSubagent |
                      ClusterSwitchOver/*???*/) =>
        // FIXME Delay CoupleController until all AttachOrder (extends OrderCommand) (and DetachOrder?) have been finished, to return a properly updated state
        agentHandle.executeCommand(command, meta.user.id, response)

      case EmergencyStop(restart) =>
        Halt.haltJava("🟥 EmergencyStop command received: JS7 AGENT STOPS NOW",
          restart = restart)


object CommandActor:
  intelliJuseImport(StringInstantJsonCodec)

  private val logger = Logger[this.type]

  object Input:
    final case class Execute(
      command: AgentCommand,
      meta: CommandMeta,
      correlId: CorrelId,
      response: Promise[Checked[Response]])

  private object Internal:
    final case class Respond(
      run: CommandRun[AgentCommand],
      correlId: CorrelId,
      promise: Promise[Checked[Response]],
      response: Try[Checked[Response]])

  final class Handle(actor: ActorRef) extends CommandHandler:
    def execute(command: AgentCommand, meta: CommandMeta): IO[Checked[Response]] =
      IO.deferFuture:
        val promise = Promise[Checked[Response]]()
        actor ! Input.Execute(command, meta, CorrelId.current, promise)
        promise.future
