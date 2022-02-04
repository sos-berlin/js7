package js7.agent.subagent

import cats.syntax.foldable._
import cats.syntax.traverse._
import js7.base.log.Logger.syntax._
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.stream.{Numbered, ObservableNumberedQueue}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import monix.eval.Task
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.{Future, Promise}
import scala.util.Try

trait CommandDispatcher
{
  protected type Command
  protected type Response

  protected def name: String
  protected val postCommand: Numbered[Command] => Task[Checked[Response]]

  private val queue = new ObservableNumberedQueue[Execute]
  private val posting = Atomic(null: Future[Unit])
  private val stopped = PublishSubject[Unit]
  private lazy val logger = Logger.withPrefix[this.type](name)

  final def start: Task[Unit] =
    Task.deferAction(scheduler => Task {
      assertThat(posting.get() == null)
      posting := processQueue.runToFuture(scheduler)
    })

  final def stop: Task[Unit] =
    Task.defer {
      posting.getAndSet(null) match {
        case null => Task.unit
        case future =>
          stopped.onComplete()
          Task.fromFuture(future)
      }
    }

  final def executeCommand(command: Command): Task[Checked[Response]] =
    executeCommands(command :: Nil)
      .map(_.head)

  final def executeCommands(commands: Iterable[Command]): Task[Seq[Checked[Response]]] = {
    val executes: Seq[Execute] = commands.view.map(new Execute(_)).toVector
    queue
      .enqueue(executes)
      .*>(executes
        .map(_.responded)
        .sequence)
  }

  private def processQueue: Task[Unit] =
    logger.debugTask(
      queue
        .observable(after = 0)
        .takeUntil(stopped)
        .flatMap(Observable.fromIterable)
        .mapEval(executeCommandNow)
        .completedL)

  private def executeCommandNow(numbered: Numbered[Execute]): Task[Unit] = {
    val execute = numbered.value
    val numberedCommand = Numbered(numbered.number, execute.command)
    postCommand(numberedCommand)
      .materialize
      .tapEval(_ => queue
        .releaseUntil(numbered.number)
        .map(_.orThrow)
        .onErrorHandle(t =>
          logger.error(s"releaseUntil(${numbered.number}) => ${t.toStringWithCauses}")))
      .map(execute.onResponse)
  }

  private final class Execute(val command: Command)
  {
    private val promise = Promise[Checked[Response]]()

    val responded = Task.fromFuture(promise.future)

    def onResponse(response: Try[Checked[Response]]): Unit =
      promise.complete(response)

    override def toString = command.toString
  }
}
