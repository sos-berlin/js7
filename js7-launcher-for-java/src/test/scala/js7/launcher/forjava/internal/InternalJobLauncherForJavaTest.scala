package js7.launcher.forjava.internal

import cats.effect.Resource
import java.lang.System.{lineSeparator as nl}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.thread.Futures.implicits.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.AlarmClock
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.{RichEitherF, RichEitherIterable, RichPartialFunction}
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.job.{InternalExecutable, JobConf, JobKey}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{NamedValue, NumericConstant, StringConstant}
import js7.data.value.expression.scopes.{FileValueScope, FileValueState}
import js7.data.value.{NamedValues, NumberValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.{Position, WorkflowBranchPath}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.forjava.internal.InternalJobLauncherForJavaTest.*
import js7.launcher.forjava.internal.tests.{TestBlockingInternalJob, TestJInternalJob}
import js7.launcher.internal.{InternalJobLauncher, JobLauncher}
import js7.launcher.process.ProcessConfiguration
import js7.launcher.{ProcessOrder, StdObservers}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.subjects.PublishSubject
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

final class InternalJobLauncherForJavaTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private val blockingThreadPoolName = "InternalJobLauncherForJavaTest"
  private val blockingJobScheduler = newUnlimitedScheduler(name = blockingThreadPoolName)

  override def afterAll() = blockingJobScheduler.shutdown()

  for (testClass <- Seq(classOf[TestJInternalJob], classOf[TestBlockingInternalJob]))
    testClass.getSimpleName - {
      lazy val executable = InternalExecutable(
        testClass.getName,
        jobArguments = Map("blockingThreadPoolName" -> StringConstant(blockingThreadPoolName)),
        arguments = Map("STEP_ARG" -> NamedValue("ORDER_ARG")))

      implicit lazy val executor: InternalJobLauncher = {
        val u = Paths.get("UNUSED")

        val jobLauncherConf = JobLauncherConf(u, u, u, u,
          UTF_8,
          killWithSigterm = ProcessConfiguration.forTest.killWithSigterm,
          killWithSigkill = ProcessConfiguration.forTest.killWithSigkill,
          killForWindows = ProcessConfiguration.forTest.killForWindows,
          None,
          scriptInjectionAllowed = true,
          RecouplingStreamReaderConf.forTest,
          globalIOX, blockingJobScheduler,
          AlarmClock())

        val jobConf = JobConf(
          JobKey(WorkflowBranchPath(WorkflowPath("WORKFLOW") ~ "1", Nil), WorkflowJob.Name("JOB")),
          WorkflowJob(AgentPath("AGENT"), executable),
          workflow,
          ControllerId("CONTROLLER"),
          sigkillDelay = 0.s,
          jobLauncherConf.systemEncoding)

        JobLauncher.checked(jobConf, jobLauncherConf)
          .orThrow.asInstanceOf[InternalJobLauncher]
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

  private def processOrder(arg: Expression)(implicit executor: InternalJobLauncher)
  : Task[Checked[(Outcome, Future[String], Future[String])]] = {
    val out, err = PublishSubject[String]()
    val outFuture = out.fold.lastOrElseL("").runToFuture
    val errFuture = err.fold.lastOrElseL("").runToFuture
    val stdObservers = new StdObservers(out, err, 4096, keepLastErrLine = false)
    temporaryDirectoryResource("InternalJobLauncherForJavaTest-")
      .flatMap(dir => Resource
        .fromAutoCloseable(Task(new FileValueState(dir)))
        .flatMap(fileValueState => FileValueScope.resource(fileValueState)))
      .use(fileValueScope =>
        executor
          .start
          .flatMapT(_ =>
            executor.toOrderProcess(
              ProcessOrder(
                Order(OrderId("TEST"), workflow.id /: Position(0),
                  Order.Processing(SubagentId("SUBAGENT"))),
                workflow,
                executor.jobConf.jobKey,
                jobResources = Nil,
                Map("ORDER_ARG" -> arg),
                ControllerId("CONTROLLER"),
                stdObservers,
                fileValueScope))
            .await(99.s).orThrow
            .start(stdObservers)
            .flatten
            .guarantee(Task {
              try out.onComplete()
              finally err.onComplete()
            })
            .map(outcome => Right((outcome, outFuture, errFuture)))))
  }
}

object InternalJobLauncherForJavaTest
{
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector.empty)
}
