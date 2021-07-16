package js7.executor.forjava.internal

import java.lang.System.{lineSeparator => nl}
import java.nio.file.Paths
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.thread.Futures.implicits._
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.{RichEitherF, RichEitherIterable, RichPartialFunction}
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.job.{InternalExecutable, JobConf, JobKey}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{NamedValue, NumericConstant, StringConstant}
import js7.data.value.{NamedValues, NumberValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.{Position, WorkflowBranchPath}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.configuration.JobExecutorConf
import js7.executor.forjava.internal.InternalJobExecutorForJavaTest._
import js7.executor.forjava.internal.tests.{TestBlockingInternalJob, TestJInternalJob}
import js7.executor.internal.{InternalJobExecutor, JobExecutor}
import js7.executor.{ProcessOrder, StdObservers}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.PublishSubject
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

final class InternalJobExecutorForJavaTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private val blockingThreadPoolName = "InternalJobExecutorForJavaTest"
  private val blockingJobScheduler = newUnlimitedScheduler(name = blockingThreadPoolName)

  override def afterAll() = blockingJobScheduler.shutdown()

  for (testClass <- Seq(classOf[TestJInternalJob], classOf[TestBlockingInternalJob]))
    testClass.getSimpleName - {
      lazy val executable = InternalExecutable(
        testClass.getName,
        jobArguments = Map("blockingThreadPoolName" -> StringConstant(blockingThreadPoolName)),
        arguments = Map("STEP_ARG" -> NamedValue("ORDER_ARG")))

      implicit lazy val executor: InternalJobExecutor = {
        val u = Paths.get("UNUSED")
        JobExecutor.checked(
          JobConf(
            JobKey(WorkflowBranchPath(WorkflowPath("WORKFLOW") ~ "1", Nil), WorkflowJob.Name("JOB")),
            WorkflowJob(AgentPath("AGENT"), executable),
            workflow,
            ControllerId("CONTROLLER"),
            sigkillDelay = 0.s,
            timeout = None),
          JobExecutorConf(u, u, u, None, scriptInjectionAllowed = true, globalIOX, blockingJobScheduler),
          _ => Left(Problem("No JobResource here"))
        ).orThrow.asInstanceOf[InternalJobExecutor]
      }

      "orderProcess" in {
        val (outcomeTask, out, err) = processOrder(NumericConstant(1000)).await(99.s).orThrow
        assert(outcomeTask == Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(1001))))
        assertOutErr(out, err)
      }

      "parallel" in {
        val indices = 1 to 1000
        val processes = for (i <- indices) yield {
          processOrder(NumericConstant(i))
            .map(_.orThrow)
            .flatMap {
              case (outcome: Outcome.Succeeded, _, _) => Task.pure(outcome.namedValues.checked("RESULT"))
              case (outcome: Outcome.NotSucceeded, _, _) => Task.pure(Left(Problem(outcome.toString)))
              case (outcome, _, _) => Task(fail(s"UNEXPECTED: $outcome"))
            }
        }
        assert(Task.parSequence(processes).await(99.s).reduceLeftEither ==
          Right(indices.map(_ + 1).map(NumberValue(_))))
      }

      "Exception is catched and returned as Left" in {
        val (outcome, out, err) = processOrder(StringConstant("INVALID TYPE")).await(99.s).orThrow
        assert(outcome.asInstanceOf[Outcome.Failed]
          .errorMessage.get startsWith "java.lang.ClassCastException")
        assertOutErr(out, err)
      }

      "stop" in {
        executor.stop.await(99.s)
        if (testClass == classOf[TestJInternalJob]) {
          assert(TestJInternalJob.stoppedCalled.containsKey(blockingThreadPoolName))
        } else if (testClass == classOf[TestBlockingInternalJob]) {
          assert(TestBlockingInternalJob.stoppedCalled.containsKey(blockingThreadPoolName))
        }
      }

      def assertOutErr(out: Future[String], err: Future[String]): Unit = {
        assert(out.await(99.s) == s"TEST FOR OUT${nl}FROM ${testClass.getName}$nl" &&
               err.await(99.s) == s"TEST FOR ERR$nl")
      }
    }

  private def processOrder(arg: Expression)(implicit executor: InternalJobExecutor)
  : Task[Checked[(Outcome, Future[String], Future[String])]] = {
    val out, err = PublishSubject[String]()
    val outFuture = out.fold.lastOrElseL("").runToFuture
    val errFuture = err.fold.lastOrElseL("").runToFuture
    val stdObservers = new StdObservers(out, err, 4096, keepLastErrLine = false)
    executor
      .start
      .flatMapT(_ =>
        Task.deferFuture(
          executor.prepareOrderProcess(
            ProcessOrder(
              Order(OrderId("TEST"), workflow.id /: Position(0), Order.Processing),
              workflow,
              executor.jobConf.jobKey,
              jobResources = Nil,
              Map("ORDER_ARG" -> arg),
              ControllerId("CONTROLLER"),
              stdObservers))
          .await(99.s).orThrow
          .runToFuture(stdObservers))
        .guarantee(Task {
          try out.onComplete()
          finally err.onComplete()
        })
        .map(outcome => Right(outcome, outFuture, errFuture)))
  }
}

object InternalJobExecutorForJavaTest
{
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector.empty)
}
