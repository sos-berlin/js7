package js7.executor.internal

import js7.base.problem.Checked._
import js7.base.thread.Futures.implicits._
import js7.base.thread.IOExecutor.globalIOX
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichPartialFunction}
import js7.data.agent.AgentId
import js7.data.job.{InternalExecutable, JobConf, JobKey}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.value.expression.Expression.{NamedValue, ObjectExpression}
import js7.data.value.{NamedValues, NumberValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.{Position, WorkflowBranchPath}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.internal.InternalJobExecutorTest._
import js7.executor.{OrderProcess, ProcessOrder, StdObservers}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.subjects.PublishSubject
import org.scalatest.freespec.AnyFreeSpec

final class InternalJobExecutorTest extends AnyFreeSpec
{
  private implicit val scheduler = Scheduler.global

  "InternalJobExecutor" in {
    val executable = InternalExecutable(
      classOf[TestInternalJob].getName,
      arguments = ObjectExpression(Map("ARG" -> NamedValue("ARG"))))
    val executor = new InternalJobExecutor(
      executable,
      JobConf(
        JobKey(WorkflowBranchPath(WorkflowPath("WORKFLOW") ~ "1", Nil), WorkflowJob.Name("JOB")),
        WorkflowJob(AgentId("AGENT"), executable),
        workflow,
        sigKillDelay = 0.s),
      globalIOX.scheduler)(Scheduler.global, globalIOX)
    val out = PublishSubject[String]()
    val err = PublishSubject[String]()
    val whenOutString = out.fold.lastL.runToFuture
    val whenErrString = err.fold.lastL.runToFuture
    val orderRun = executor.toOrderProcess(ProcessOrder(
      Order(OrderId("TEST"), workflow.id /: Position(0), Order.Processing),
      workflow,
      NamedValues("ARG" -> NumberValue(1)),
      StdObservers(out, err, charBufferSize = 4096))
    ).orThrow
    val outcome = orderRun.runToFuture.await(99.s)
    assert(outcome == Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(2))))
    out.onComplete()
    err.onComplete()
    assert(whenOutString.await(99.s) == "OUT 1/" + "OUT 2" &&
           whenErrString.await(99.s) == "ERR 1/" + "ERR 2")
  }
}

object InternalJobExecutorTest
{
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector.empty)

  final class TestInternalJob extends InternalJob
  {
    override def toOrderProcess(step: Step) =
      OrderProcess(
        Task.fromFuture(step.outObserver.onNext("OUT 1/")) >>
        Task.fromFuture(step.errObserver.onNext("ERR 1/")) >>
        Task.fromFuture(step.outObserver.onNext("OUT 2")) >>
        Task.fromFuture(step.errObserver.onNext("ERR 2")) >>
        Task {
          Outcome.Completed.fromChecked(
          step.arguments.checked("ARG")
            .flatMap(_.narrow[NumberValue])
            .map(_.number + 1)
            .map(result => Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(result)))))
        }
      )
  }
}
