package js7.executor.forjava.internal

import java.lang.System.lineSeparator
import js7.base.problem.Checked._
import js7.base.thread.Futures.implicits._
import js7.base.thread.IOExecutor.Implicits.globalIOX
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
import js7.executor.internal.InternalJob.{OrderContext, Result}
import js7.executor.internal.{InternalExecutor, InternalJob}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.PublishSubject
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

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
        val (orderProcess, out, err) = processOrder(NumberValue(1000))
        assert(orderProcess.completed.await(99.s) ==
          Right(Result(NamedValues("RESULT" -> NumberValue(1001)))))
        assertOutErr(out, err)
      }

      "parallel" in {
        val indices = 1 to 1000
        val processes = for (i <- indices) yield {
          val (orderProcess, out, err) = processOrder(NumberValue(i))
          orderProcess.completed.flatMapT(result => Task.pure(result
            .namedValues.checked("RESULT")))
        }
        assert(Task.parSequence(processes).await(99.s).reduceLeftEither ==
          Right(indices.map(_ + 1).map(NumberValue(_))))
      }

      "Exception is catched and returned as Left" in {
        val (orderProcess, out, err) = processOrder(StringValue("INVALID TYPE"))
        assert(orderProcess.completed.await(99.s)
          .swap.orThrow.toString startsWith "java.lang.ClassCastException")
        assertOutErr(out, err)
      }

      def assertOutErr(out: Future[String], err: Future[String]): Unit = {
        assert(out.await(99.s) == s"TEST FOR OUT${lineSeparator}FROM ${testClass.getName}\n" &&
               err.await(99.s) == s"TEST FOR ERR")
      }
    }

  private def processOrder(arg: Value)(implicit executor: InternalExecutor)
  : (InternalJob.OrderProcess, Future[String], Future[String]) = {
    val out, err = PublishSubject[String]()
    val outTask = out.fold.lastOrElseL("").runToFuture
    val errTask = err.fold.lastOrElseL("").runToFuture
    val process = executor
      .processOrder(OrderContext(
        Order(OrderId("TEST"), workflow.id /: Position(0), Order.Processing),
        workflow,
        NamedValues("arg" -> arg),
        Scope.empty,
        out, err))
      .await(99.s).orThrow
    (process, outTask, errTask)
  }
}

object InternalExecutorForJavaTest
{
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector.empty)
}
