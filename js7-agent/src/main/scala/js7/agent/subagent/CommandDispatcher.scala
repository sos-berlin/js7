package js7.agent.subagent

import cats.syntax.traverse._
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.Switch
import js7.base.problem.Checked
import js7.base.stream.{Numbered, ObservableNumberedQueue}
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax._
import js7.data.command.CommonCommand
import js7.data.subagent.SubagentRunId
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.{Future, Promise}
import scala.util.Try

trait CommandDispatcher
{
  protected type Command <: CommonCommand
  private type Response = Unit
  protected final type PostCommand = (Numbered[Command], SubagentRunId, Switch.ReadOnly) =>
    Task[Checked[Response]]

  protected def name: String
  protected val postCommand: PostCommand

  private lazy val logger = Logger.withPrefix[this.type](name)
  private val queue = new ObservableNumberedQueue[Execute]
  private val processingAllowed = Switch(false)
  @volatile
  private var processing: Future[Unit] = Future.successful(())
  private val lock = AsyncLock()

  def start(subagentRunId: SubagentRunId): Task[Unit] = {
    lock.lock(
      processingAllowed.switchOnThen(
        logger.debugTask(
          Task.deferAction(scheduler => Task {
            processing = processQueue(subagentRunId).runToFuture(scheduler)
          }))))
  }

  def stop: Task[Unit] =
    lock.lock(
      processingAllowed.switchOffThen(logger.debugTask(
        queue.dequeueAll
          .*>(Task.fromFuture(processing))
          .<*(Task {
            processing = Future.successful(())
          }))))

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
    logger.traceTask("enqueueCommands",
      executes.headOption.fold("")(_.command.toShortString) + ((executes.sizeIs > 1) ?? ",...")
    )(queue
      .enqueue(executes)
      .map(_ => executes.map(_.responded)))
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
      .map(execute.onResponse)
  }

  override def toString = s"CommandDispatcher($name)"

  private final class Execute(val command: Command)
  {
    private val promise = Promise[Checked[Response]]()

    val responded = Task.fromFuture(promise.future)

    def onResponse(response: Try[Checked[Response]]): Unit =
      promise.complete(response)

    override def toString = s"Execute(${command.toShortString}"
  }
}
