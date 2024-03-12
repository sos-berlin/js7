package js7.launcher.forjava.internal

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import java.lang.System.lineSeparator as nl
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths
import js7.base.catsutils.CatsEffectExtensions.{joinStd, left}
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.parSequence
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.time.AlarmClock
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.system.ThreadPools
import js7.common.system.ThreadPools.newUnlimitedNonVirtualExecutionContext
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.job.{InternalExecutable, JobConf, JobKey, ShellScriptExecutable}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{NamedValue, NumericConstant, StringConstant}
import js7.data.value.expression.scopes.{FileValueScope, FileValueState}
import js7.data.value.{NamedValues, NumberValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.{Position, WorkflowBranchPath}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.StdObserversForTest.testSink
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.forjava.internal.InternalJobLauncherForJavaTest.*
import js7.launcher.forjava.internal.tests.{TestBlockingInternalJob, TestJInternalJob}
import js7.launcher.internal.{InternalJobLauncher, JobLauncher}
import js7.launcher.process.ProcessConfiguration
import js7.launcher.{ProcessOrder, StdObservers}
import org.scalatest.BeforeAndAfterAll

final class InternalJobLauncherForJavaTest extends OurTestSuite, BeforeAndAfterAll
{
  private given IORuntime = ioRuntime

  private val blockingJobEC =
    newUnlimitedNonVirtualExecutionContext(
      name = blockingThreadNamePrefix + "-AGENT blocking-job"/*like Subagent.resource*/)

  override def afterAll() =
    try
      blockingJobEC.shutdown()
    finally
      super.afterAll()

  for testClass <- Seq(classOf[TestJInternalJob], classOf[TestBlockingInternalJob]) do
    testClass.getSimpleName - {
      lazy val executable = InternalExecutable(
        testClass.getName,
        script = "TEST SCRIPT",
        jobArguments = Map("blockingThreadNamePrefix" -> StringConstant(blockingThreadNamePrefix)),
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
          errorLineLengthMax = 1024,
          RecouplingStreamReaderConf.forTest,
          globalIOX, blockingJobEC = blockingJobEC,
          null: AlarmClock/*AlarmClock()*/)

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
        val (outcomeIO, out, err) = processOrder(NumericConstant(1000)).await(99.s).orThrow
        assert(outcomeIO == Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(1001))))
        assertOutErr(out, err)
      }

      "parallel" in {
        val indices = 1 to 1000
        val processes = for i <- indices yield {
          processOrder(NumericConstant(i))
            .map(_.orThrow)
            .flatMap {
              case (outcome: Outcome.Succeeded, _, _) => IO.pure(outcome.namedValues.checked("RESULT"))
              case (outcome: Outcome.NotSucceeded, _, _) => IO.left(Problem(outcome.toString))
              case (outcome, _, _) => IO(fail(s"UNEXPECTED: $outcome"))
            }
        }
        assert(IO.parSequence(processes).await(99.s).reduceLeftEither ==
          Right(indices.map(_ + 1).map(NumberValue(_))))
      }

      "Exception is caught and returned as Left" in {
        val (outcome, out, err) = processOrder(StringConstant("INVALID TYPE")).await(99.s).orThrow
        assert(outcome.asInstanceOf[Outcome.Failed]
          .errorMessage.get startsWith "java.lang.ClassCastException")
        assertOutErr(out, err)
      }

      "stop" in {
        executor.stop.await(99.s)
        if testClass == classOf[TestJInternalJob] then {
          assert(TestJInternalJob.stoppedCalled.containsKey(blockingThreadNamePrefix))
        } else if testClass == classOf[TestBlockingInternalJob] then {
          logger.info(s"TestBlockingInternalJob.stoppedCalled=${TestBlockingInternalJob.stoppedCalled}")
          assert(TestBlockingInternalJob.stoppedCalled.containsKey(blockingThreadNamePrefix))
        }
      }

      def assertOutErr(out: String, err: String): Unit = {
        assert(out == s"TEST FOR OUT${nl}FROM ${testClass.getName}$nl" &&
               err == s"TEST FOR ERR$nl")
      }
    }

  private def processOrder(arg: Expression)(implicit launcher: InternalJobLauncher)
  : IO[Checked[(Outcome, String, String)]] =
    val orderId = OrderId("TEST")
    val jobKey = launcher.jobConf.jobKey

    (for
      testSink <- StdObservers.testSink(4096, name = "InternalJobLauncherForJavaTest")
      dir <- temporaryDirectoryResource[IO]("InternalJobLauncherForJavaTest-")
      fileValueScope <- Resource
        .fromAutoCloseable(IO(new FileValueState(dir)))
        .flatMap(FileValueScope.resource)
    yield (testSink, fileValueScope))
      .use: (testSink, fileValueScope) =>
        for
          orderProcess <- launcher
            .start
            .flatMapT(_ => launcher.toOrderProcess:
              ProcessOrder(
                Order(orderId, workflow.id /: Position(0),
                  Order.Processing(SubagentId("SUBAGENT"))),
                workflow,
                jobKey,
                WorkflowJob(AgentPath("AGENT"), ShellScriptExecutable("")),
                jobResources = Nil,
                executeArguments = Map.empty,
                jobArguments = Map("ORDER_ARG" -> arg),
                ControllerId("CONTROLLER"),
                testSink.stdObservers,
                fileValueScope))
            .map(_.orThrow)
          orderOutcome <- orderProcess
            .start(orderId, jobKey)
            .flatMap(_.joinStd)
          _ <- testSink.stdObservers.closeChannels
          out <- testSink.out
          err <- testSink.err
        yield
          Right((orderOutcome, out, err))
}


object InternalJobLauncherForJavaTest
{
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector.empty)
  private val logger = Logger[this.type]
}
