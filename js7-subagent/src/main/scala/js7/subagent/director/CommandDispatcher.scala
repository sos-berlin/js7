package js7.subagent.director

import cats.effect.kernel.Deferred
import cats.effect.{FiberIO, IO}
import cats.syntax.traverse.*
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.catsutils.CatsExtensions.tryIt
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.completedL
import js7.base.monixutils.Switch
import js7.base.problem.{Checked, Problem}
import js7.base.stream.{Numbered, StreamNumberedQueue}
import js7.base.utils.AsyncLock
import js7.base.utils.CatsUtils.pureFiberIO
import js7.base.utils.ScalaUtils.=>?
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.command.CommonCommand
import js7.data.subagent.SubagentRunId
import js7.subagent.director.CommandDispatcher.*
import scala.util.{Success, Try}

private trait CommandDispatcher:
  protected type Command <: CommonCommand
  protected type Response
  protected final type PostCommand = (Numbered[Command], SubagentRunId, Switch.ReadOnly) =>
    IO[Checked[Response]]

  protected def name: String
  protected val postCommand: PostCommand

  private lazy val logger = Logger.withPrefix[this.type](name)
  private var queue = new StreamNumberedQueue[Execute]
  private val processingAllowed = Switch(false)
  private var processing: FiberIO[Unit] = pureFiberIO(()) // TODO Maybe unsafe immutable
  private val lock = AsyncLock()

  def start(subagentRunId: SubagentRunId): IO[Unit] =
    lock.lock:
      processingAllowed.switchOnThen:
        logger.debugIO:
          for fiber <- processQueue(subagentRunId).start yield
            processing = fiber

  /** Stop and forget queued commands (do not respond them). */
  def shutdown: IO[Unit] =
    stopWithResponse(PartialFunction.empty)

  def stopAndFailCommands: IO[Unit] =
    stopWithResponse: _ =>
      Left(StoppedProblem)

  def stopWithResponse(commandToResponse: Command =>? Checked[Response]): IO[Unit] =
    lock.lock:
      processingAllowed.switchOff *>
        logger.traceIO:
          queue.stop.flatMap: numberedExecutes =>
            queue = new StreamNumberedQueue[Execute]
            numberedExecutes.foldMap: numberedExecute =>
              commandToResponse.lift(numberedExecute.value.command) match
                case None =>
                  IO(logger.debug(s"⚠️  stopWithResponse $numberedExecute => discarded"))

                case Some(response) =>
                  logger.debug(s"⚠️  stopWithResponse $numberedExecute => $response")
                  numberedExecute.value.tryRespond(Success(response))
            // TODO Die anderen Kommandos auch abbrechen? tryResponse(Success(Left(??)))
            .productR:
              processing.joinStd

  final def executeCommand(command: Command): IO[Checked[Response]] =
    executeCommands(command :: Nil)
      .map(_.head)

  private def executeCommands(commands: Iterable[Command]): IO[Seq[Checked[Response]]] =
    enqueueCommands(commands)
      .flatMap(_.sequence)

  final def enqueueCommand(command: Command): IO[IO[Checked[Response]]] =
    enqueueCommands(command :: Nil)
      .map(_.head)

  private def enqueueCommands(commands: Iterable[Command]): IO[Seq[IO[Checked[Response]]]] =
    val executes: Seq[Execute] = commands.view.map(new Execute(_)).toVector
    queue.enqueue(executes).as:
      executes.map(_.responded)

  private def processQueue(subagentRunId: SubagentRunId): IO[Unit] =
    logger.debugIO:
      IO.defer:
        // Save queue, because it may changes with stop and start
        val queue = this.queue
        queue
          .stream
          .interruptWhenF(processingAllowed.whenOff)
          .evalMap: numbered =>
            numbered.value.correlId.bind:
              executeCommandNow(subagentRunId, numbered)
                .tryIt
                .flatTap(_ => queue
                  .release(numbered.number)
                  .map(_.orThrow)
                  .handleError: t =>
                    logger.error(s"release(${numbered.number}) => ${t.toStringWithCauses}"))
                .flatMap:
                  numbered.value/*Execute*/.respond
          .completedL

  private def executeCommandNow(subagentRunId: SubagentRunId, numbered: Numbered[Execute])
  : IO[Checked[Response]] =
    numbered.value.correlId.bind:
      val numberedCommand = Numbered(numbered.number, numbered.value.command)
      postCommand(numberedCommand, subagentRunId, processingAllowed/*stop retrying when off*/)

  override def toString = s"CommandDispatcher($name)"

  private final class Execute(
    val command: Command,
    val whenResponded: Deferred[IO, Try[Checked[Response]]] =
      Deferred.unsafe[IO, Try[Checked[Response]]],
    val correlId: CorrelId = CorrelId.current):

    val responded: IO[Checked[Response]] =
      whenResponded.get.flatMap(IO.fromTry)

    def respond(response: Try[Checked[Response]]): IO[Unit] =
      whenResponded.complete(response).flatMap:
        IO.unlessA(_):
          whenResponded.get.map: tried =>
            logger.warn/*debug?*/:
              s"response($response): command already responded: ${command.toShortString} => $tried"

    def tryRespond(response: Try[Checked[Response]]): IO[Unit] =
      whenResponded.complete(response).void

    override def toString = s"Execute(${command.toShortString})"


object CommandDispatcher:
  private[director] val StoppedProblem = Problem.pure("CommandDispatcher stopped")
