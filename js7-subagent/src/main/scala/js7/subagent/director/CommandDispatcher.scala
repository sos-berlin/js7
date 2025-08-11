package js7.subagent.director

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
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.command.CommonCommand
import js7.data.subagent.SubagentRunId
import js7.subagent.director.CommandDispatcher.*
import scala.concurrent.Promise
import scala.util.{Success, Try}

private trait CommandDispatcher:
  protected type Command <: CommonCommand
  protected type Response
  protected final type PostCommand = (Numbered[Command], SubagentRunId, Switch.ReadOnly) =>
    IO[Checked[Response]]

  protected def name: String
  protected val postCommand: PostCommand

  protected final var queue = new StreamNumberedQueue[Execute]
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
    stopWithProblem(PartialFunction.empty)

  def stopAndFailCommands: IO[Unit] =
    stopWithProblem:
      case _ => StoppedProblem

  private def stopWithProblem(
    commandToProblem: PartialFunction[Command, Problem] = PartialFunction.empty)
  : IO[Unit] =
    lock.lock:
      processingAllowed.switchOff *>
        logger.debugIO:
          queue.stop.flatMap: numberedExecutes =>
            queue = new StreamNumberedQueue[Execute]
            for numberedExecute <- numberedExecutes do
              commandToProblem.lift(numberedExecute.value.command) match
                case None =>
                  logger.debug(s"⚠️  stopWithProblem $numberedExecute => discarded")

                case Some(problem) =>
                  logger.debug(s"⚠️  stopWithProblem $numberedExecute => $problem")
                  numberedExecute.value.tryRespond(Success(Left(problem)))
            // TODO Die anderen Kommandos auch abbrechen? tryResponse(Success(Left(??)))
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
    queue
      .enqueue(executes)
      .map(_ => executes.map(_.responded))

  private def processQueue(subagentRunId: SubagentRunId): IO[Unit] =
    logger.debugIO:
      IO.defer:
        // Save queue, because it may changes with stop and start
        val queue = this.queue
        queue
          .stream
          .interruptWhenF(processingAllowed.whenOff)
          .evalMap(numbered =>
            numbered.value.correlId.bind(
              executeCommandNow(subagentRunId, numbered)
                .tryIt
                .flatTap(_ => queue
                  .release(numbered.number)
                  .map(_.orThrow)
                  .handleError(t =>
                    logger.error(s"release(${numbered.number}) => ${t.toStringWithCauses}")))
                .map(numbered.value/*Execute*/.respond)))
          .completedL

  private def executeCommandNow(subagentRunId: SubagentRunId, numbered: Numbered[Execute])
  : IO[Checked[Response]] =
    numbered.value.correlId.bind:
      val numberedCommand = Numbered(numbered.number, numbered.value.command)
      postCommand(numberedCommand, subagentRunId, processingAllowed/*stop retrying when off*/)

  override def toString = s"CommandDispatcher($name)"

  private final class Execute(
    val command: Command,
    val promise: Promise[Checked[Response]] = Promise(),
    val correlId: CorrelId = CorrelId.current):
    val responded: IO[Checked[Response]] = IO.fromFuture(IO.pure(promise.future))

    def respond(response: Try[Checked[Response]]): Unit =
      if !promise.tryComplete(response) then
        logger.warn/*debug?*/(
          s"response($response): command already responded: ${command.toShortString} => ${promise.future.value.get}")

    def tryRespond(response: Try[Checked[Response]]): Unit =
      promise.tryComplete(response)

    override def toString = s"Execute(${command.toShortString})"


object CommandDispatcher:
  private val logger = Logger[this.type]
  private[director] val StoppedProblem = Problem.pure("CommandDispatcher stopped")
