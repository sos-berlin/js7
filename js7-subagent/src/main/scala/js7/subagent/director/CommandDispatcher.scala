package js7.subagent.director

import cats.syntax.traverse.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.Switch
import js7.base.problem.{Checked, Problem}
import js7.base.stream.{Numbered, ObservableNumberedQueue}
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.command.CommonCommand
import js7.data.subagent.SubagentRunId
import js7.subagent.director.CommandDispatcher.*
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.{Future, Promise}
import scala.util.{Success, Try}

private trait CommandDispatcher:
  protected type Command <: CommonCommand
  private type Response = Unit
  protected final type PostCommand = (Numbered[Command], SubagentRunId, Switch.ReadOnly) =>
    Task[Checked[Response]]

  protected def name: String
  protected val postCommand: PostCommand

  private lazy val logger = Logger.withPrefix[this.type](name)
  protected final var queue = new ObservableNumberedQueue[Execute]
  private val processingAllowed = Switch(false)
  private var processing: Future[Unit] = Future.successful(())
  private val lock = AsyncLock()

  def start(subagentRunId: SubagentRunId): Task[Unit] =
    lock.lock(
      processingAllowed.switchOnThen(
        logger.debugTask(
          Task.deferAction(scheduler => Task {
            processing = processQueue(subagentRunId).runToFuture(scheduler)
          }))))

  /** Stop and forget queued commands (do not respond them). */
  def shutdown: Task[Unit] =
    stopWithProblem(PartialFunction.empty)

  def stopAndFailCommands: Task[Unit] =
    stopWithProblem:
      case _ => StoppedProblem

  private def stopWithProblem(
    commandToProblem: PartialFunction[Command, Problem] = PartialFunction.empty)
  : Task[Unit] =
    lock.lock(
      processingAllowed.switchOff.*>(logger.debugTask(
        queue.stop
          .flatMap { numberedExecutes =>
            queue = new ObservableNumberedQueue[Execute]
            for numberedExecute <- numberedExecutes do {
              commandToProblem.lift(numberedExecute.value.command) match {
                case None =>
                  logger.debug(s"⚠️ $numberedExecute => discarded")

                case Some(problem) =>
                  logger.debug(s"⚠️ $numberedExecute => $problem")
                  numberedExecute.value.tryRespond(Success(Left(problem)))
              }
              // TODO Die anderen Kommandos auch abbrechen? tryResponse(Success(Left(??)))
            }
            Task.fromFuture(processing)
              .<*(Task {
                processing = Future.successful(())
              })
          })))

  //def dequeueAll: Task[Seq[Numbered[Command]]] =
  //  queue.dequeueAll.map(_.map(_.map(_.command)))

  final def executeCommand(command: Command): Task[Checked[Response]] =
    executeCommands(command :: Nil)
      .map(_.head)

  private def executeCommands(commands: Iterable[Command]): Task[Seq[Checked[Response]]] =
    enqueueCommands(commands)
      .flatMap(_.sequence)

  final def enqueueCommand(command: Command): Task[Task[Checked[Response]]] =
    enqueueCommands(command :: Nil)
      .map(_.head)

  private def enqueueCommands(commands: Iterable[Command]): Task[Seq[Task[Checked[Response]]]] =
    val executes: Seq[Execute] = commands.view.map(new Execute(_)).toVector
    queue
      .enqueue(executes)
      .map(_ => executes.map(_.responded))

  private def processQueue(subagentRunId: SubagentRunId): Task[Unit] =
    logger.debugTask(Task.defer {
      // Save queue, because it may changes with stop and start
      val queue = this.queue
      queue
        .observable
        .takeUntilEval(processingAllowed.whenOff)
        .flatMap(Observable.fromIterable)
        .mapEval(numbered =>
          numbered.value.correlId.bind(
            executeCommandNow(subagentRunId, numbered)
              .materialize
              .tapEval(_ => queue
                .release(numbered.number)
                .map(_.orThrow)
                .onErrorHandle(t =>
                  logger.error(s"release(${numbered.number}) => ${t.toStringWithCauses}")))
              .map(numbered.value/*Execute*/.respond)))
        .completedL
    })

  private def executeCommandNow(subagentRunId: SubagentRunId, numbered: Numbered[Execute])
  : Task[Checked[Response]] =
    numbered.value.correlId.bind:
      val numberedCommand = Numbered(numbered.number, numbered.value.command)
      postCommand(numberedCommand, subagentRunId, processingAllowed/*stop retrying when off*/)

  override def toString = s"CommandDispatcher($name)"

  protected final class Execute(
    val command: Command,
    val promise: Promise[Checked[Response]] = Promise(),
    val correlId: CorrelId = CorrelId.current):
    val responded = Task.fromFuture(promise.future)

    def respond(response: Try[Checked[Response]]): Unit =
      if !promise.tryComplete(response) then
        logger.warn/*debug?*/(
          s"response($response): command already responded: ${command.toShortString} => ${promise.future.value.get}")

    def tryRespond(response: Try[Checked[Response]]): Unit =
      promise.tryComplete(response)

    override def toString = s"Execute(${command.toShortString})"


object CommandDispatcher:
  private[director] val StoppedProblem = Problem.pure("CommandDispatcher stopped")
