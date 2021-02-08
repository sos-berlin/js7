package js7.executor.internal

import js7.base.problem.Checked._
import js7.base.thread.IOExecutor.globalIOX
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichPartialFunction}
import js7.data.job.InternalExecutable
import js7.data.order.{Order, OrderId}
import js7.data.value.expression.Scope
import js7.data.value.{NamedValues, NumberValue}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.internal.InternalExecutorTest._
import js7.executor.internal.InternalJob.{OrderContext, OrderProcess, Result}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class InternalExecutorTest extends AnyFreeSpec
{
  "InternalExecutor" in {
    val executable = InternalExecutable(classOf[TestInternalJob].getName)
    val executor = new InternalExecutor(executable, globalIOX.testScheduler)
    val orderProcess = executor.processOrder(OrderContext(
      Order(OrderId("TEST"), workflow.id /: Position(0), Order.Processing),
      workflow,
      NamedValues("ARG" -> NumberValue(1)),
      Scope.empty)).await(99.s).orThrow
    assert(orderProcess.completed.await(99.s) ==
      Right(Result(NamedValues("RESULT" -> NumberValue(2)))))
  }
}

object InternalExecutorTest
{
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector.empty)

  final class TestInternalJob extends InternalJob
  {
    override def processOrder(context: OrderContext) =
      OrderProcess(
        Task {
          context.arguments.checked("ARG")
            .flatMap(_.narrow[NumberValue])
            .map(_.number + 1)
            .map(result =>
              Result(NamedValues("RESULT" -> NumberValue(result))))
        }
      )
  }
}
