package js7.executor.forjava.internal

import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichEitherF, RichEitherIterable, RichPartialFunction}
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.data.job.InternalExecutable
import js7.data.order.{Order, OrderId}
import js7.data.value.expression.Scope
import js7.data.value.{NamedValues, NumberValue, StringValue, Value}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.forjava.internal.InternalExecutorForJavaTest._
import js7.executor.forjava.internal.tests.{TestBlockingInternalJob, TestJInternalJob}
import js7.executor.internal.InternalExecutor
import js7.executor.internal.InternalJob.{OrderContext, Result}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec

final class InternalExecutorForJavaTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private val threadPoolName = "InternalExecutorForJavaTest"
  private val jobScheduler = newUnlimitedScheduler(name = threadPoolName)

  override def afterAll() = jobScheduler.shutdown()

  for (testClass <- Seq(classOf[TestJInternalJob], classOf[TestBlockingInternalJob]))
    testClass.getSimpleName - {
      implicit val executor = new InternalExecutor(
        InternalExecutable(
          testClass.getName,
          Map("expectedThreadPrefix" -> StringValue(s"$threadPoolName "))
        ),
        jobScheduler)

      "processOrder" in {
        for (i <- 1 to 3) withClue(s"Order #$i: ") {
          assert(processOrder(NumberValue(i)).completed.await(99.s) ==
            Right(Result(NamedValues("RESULT" -> NumberValue(i + 1)))))
        }
      }

      "parallel" in {
        val indices = 1 to 10000
        val processes = for (i <- indices) yield
          processOrder(NumberValue(i))
            .completed
            .flatMapT(result => Task.pure(result
              .namedValues.checked("RESULT")))
        assert(Task.parSequence(processes).await(99.s).reduceLeftEither ==
          Right(indices.map(_ + 1).map(NumberValue(_))))
      }

      "Exception is catched and returned as Left" in {
        assert(processOrder(StringValue("INVALID TYPE")).completed.await(99.s)
          .swap.orThrow.toString startsWith "java.lang.ClassCastException")
      }
    }

  private def processOrder(arg: Value)(implicit executor: InternalExecutor) =
    executor.processOrder(OrderContext(
      Order(OrderId("TEST"), workflow.id /: Position(0), Order.Processing),
      workflow,
      NamedValues("arg" -> arg),
      Scope.empty)
    ).await(99.s).orThrow
}

object InternalExecutorForJavaTest
{
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector.empty)
}
