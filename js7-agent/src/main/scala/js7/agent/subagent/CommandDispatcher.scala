package js7.agent.subagent

import cats.syntax.traverse._
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.Switch
import js7.base.problem.{Checked, Problem}
import js7.base.stream.{Numbered, ObservableNumberedQueue}
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax._
import js7.data.command.CommonCommand
import js7.data.subagent.SubagentRunId
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.{Future, Promise}
import scala.util.{Success, Try}

private trait CommandDispatcher
{
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

  def stop(commandToProblem: PartialFunction[Command, Problem] = PartialFunction.empty)
  : Task[Unit] =
    lock.lock(
      processingAllowed.switchOff.*>(logger.debugTask(
        queue.dequeueAll
          .flatMap { numberedExecutes =>
            queue = new ObservableNumberedQueue[Execute]
            for (numberedExecute <- numberedExecutes) {
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

  final def executeCommands(commands: Iterable[Command]): Task[Seq[Checked[Response]]] =
    enqueueCommands(commands)
      .flatMap(_.sequence)

  final def enqueueCommand(command: Command): Task[Task[Checked[Response]]] =
    enqueueCommands(command :: Nil)
      .map(_.head)

  def enqueueCommands(commands: Iterable[Command]): Task[Seq[Task[Checked[Response]]]] = {
    val executes: Seq[Execute] = commands.view.map(new Execute(_)).toVector
    queue
      .enqueue(executes)
      .map(_ => executes.map(_.responded))
  }

  private def processQueue(subagentRunId: SubagentRunId): Task[Unit] =
    logger.debugTask(
      queue
        .observable
        .takeUntilEval(processingAllowed.whenOff)
        .flatMap(Observable.fromIterable)
        .mapEval(executeCommandNow(subagentRunId, _))
        .completedL)

  private def executeCommandNow(subagentRunId: SubagentRunId, numbered: Numbered[Execute])
  : Task[Unit] = {
    val execute = numbered.value
    val numberedCommand = Numbered(numbered.number, execute.command)
    postCommand(numberedCommand, subagentRunId, processingAllowed/*stop retrying when off*/)
      .materialize
      .tapEval(_ => queue
        .releaseUntil(numbered.number)
        .map(_.orThrow)
        .onErrorHandle(t =>
          logger.error(s"releaseUntil(${numbered.number}) => ${t.toStringWithCauses}")))
      .map(execute.respond)
  }

  override def toString = s"CommandDispatcher($name)"

  protected final class Execute(
    val command: Command,
    val promise: Promise[Checked[Response]] = Promise())
  {
    val responded = Task.fromFuture(promise.future)

    def respond(response: Try[Checked[Response]]): Unit =
      if (!promise.tryComplete(response)) {
        logger.warn/*debug?*/(
          s"response($response): command already responded: ${command.toShortString} => ${promise.future.value.get}")
      }

    def tryRespond(response: Try[Checked[Response]]): Unit =
      promise.tryComplete(response)

    override def toString = s"Execute(${command.toShortString})"
  }
}
