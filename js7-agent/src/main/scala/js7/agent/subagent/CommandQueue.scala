package js7.agent.subagent

import cats.syntax.foldable._
import cats.syntax.traverse._
import js7.agent.subagent.CommandQueue._
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.stream.{Numbered, ObservableNumberedQueue}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.HttpClient
import js7.data.item.InventoryItem
import js7.data.order.Order
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression
import js7.subagent.data.SubagentCommand
import js7.subagent.data.SubagentCommand.{AttachItem, StartOrderProcess}
import monix.eval.Task
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.{Future, Promise}
import scala.util.Try

private final class CommandQueue(
  subagentId: SubagentId,
  postCommand: Numbered[SubagentCommand] => Task[SubagentCommand.Response])
{
  private val queue = new ObservableNumberedQueue[Execute]
  private val posting = Atomic(null: Future[Unit])
  private val stopped = PublishSubject[Unit]
  private val logger = Logger.withPrefix[this.type](subagentId.toString)

  def start: Task[Unit] =
    Task.deferAction(scheduler => Task {
      assertThat(posting.get() == null)
      posting := processQueue.runToFuture(scheduler)
    })

  def stop: Task[Unit] =
    Task.defer {
      posting.getAndSet(null) match {
        case null => Task.unit
        case future =>
          stopped.onComplete()
          Task.fromFuture(future)
      }
    }

  def startProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression],
    items: Iterable[InventoryItem])
  : Task[Checked[Unit]] =
    executeCommands(items.view.map(AttachItem(_)))
      .flatMapT(_ =>
        executeCommands(StartOrderProcess(order, defaultArguments) :: Nil)
          .map(Right(_)))

  def executeCommand(command: SubagentCommand): Task[Checked[Unit]] =
    executeCommands(command :: Nil)

  def executeCommands(commands: Iterable[SubagentCommand]): Task[Checked[Unit]] = {
    val executes = commands.view.map(new Execute(_)).toVector
    queue
      .enqueue(executes)
      .*>(executes
        .map(_.responded)
        .sequence
        .map(_
          .map(_.rightAs(()))
          .combineAll))
  }

  private def processQueue: Task[Unit] =
    queue
      .observable(after = 0)
      .takeUntil(stopped)
      .flatMap(Observable.fromIterable)
      .mapEval(executeCommandNow)
      .completedL
      .guaranteeCase(exitCase => Task(
        logger.debug(s"Observing commands => $exitCase")))

  private def executeCommandNow(numbered: Numbered[Execute]): Task[Unit] = {
    val execute = numbered.value
    val numberedCommand = Numbered(numbered.number, execute.command)
    HttpClient
      .liftProblem(
        postCommand(numberedCommand))
      .materialize
      .tapEval(_ => queue
        .releaseUntil(numbered.number)
        .map(_.orThrow)
        .onErrorHandle(t =>
          logger.error(s"releaseUntil(${numbered.number}) => ${t.toStringWithCauses}")))
      .map(execute.onResponse)
  }
}

private object CommandQueue
{
  private final class Execute(val command: SubagentCommand)
  {
    private val promise = Promise[Checked[SubagentCommand.Response]]()

    val responded = Task.fromFuture(promise.future)

    def onResponse(response: Try[Checked[SubagentCommand.Response]]): Unit =
      promise.complete(response)

    override def toString = command.toString
  }
}
