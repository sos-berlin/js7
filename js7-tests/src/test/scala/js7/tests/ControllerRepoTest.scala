package js7.tests

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.problem.Problems.DuplicateKey
import js7.base.system.OperatingSystem.isWindows
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.RunningController
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerCommand.UpdateRepo
import js7.data.Problems.ItemDeletedProblem
import js7.data.agent.AgentName
import js7.data.item.VersionId
import js7.data.job.RelativeExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import js7.tests.testenv.DirectoryProvider
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Try

final class ControllerRepoTest extends AnyFreeSpec
{
  import ControllerRepoTest._

  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentName), testName = Some("ControllerRepoTest"))) { provider =>
      import provider.itemSigner

      for (v <- 1 to 4)  // For each version, we use a dedicated job which echos the VersionId
        provider.agents.head.writeExecutable(RelativeExecutablePath(s"EXECUTABLE-V$v$sh"), (isWindows ?? "@") + s"echo /VERSION-$v/")
      provider.controller.configDir / "controller.conf" ++=
        """js7.auth.users.TEST-USER {
          |  password = "plain:TEST-PASSWORD"
          |  permissions = [ UpdateRepo ]
          |}
          |""".stripMargin

      provider.runAgents() { _ =>
        provider.runController() { controller =>
          controller.httpApi.login_(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))).await(99.s)

          locally {
            // Add Workflow
            val v = V1
            val workflow = testWorkflow(v) withId AWorkflowPath ~ v
            val signedString = itemSigner.sign(workflow)
            controller.executeCommandAsSystemUser(UpdateRepo(v, Seq(signedString))).await(99.s).orThrow
            controller.runOrder(FreshOrder(OrderId("A"), workflow.path))

            // Non-empty UpdateRepo with same resulting Repo is accepted
            controller.executeCommandAsSystemUser(UpdateRepo(v, Seq(signedString))).await(99.s).orThrow
            controller.executeCommandAsSystemUser(UpdateRepo(v, Seq(signedString))).await(99.s).orThrow

            // Empty UpdateRepo with same VersionId is rejected due to duplicate VersionId
            assert(controller.executeCommandAsSystemUser(UpdateRepo(v)).await(99.s) ==
              Left(DuplicateKey("VersionId", v)))
          }

          locally {
            // Add another Workflow
            val v = V2
            val workflow = testWorkflow(v) withId BWorkflowPath ~ v
            val signedString = itemSigner.sign(workflow)
            controller.executeCommandAsSystemUser(UpdateRepo(v, Seq(signedString))).await(99.s).orThrow
            controller.runOrder(FreshOrder(OrderId("B"), workflow.path))
          }

          // Change first Workflow
          locally {
            val v = V3
            val workflow = testWorkflow(v) withId AWorkflowPath ~ v
            controller.executeCommandAsSystemUser(UpdateRepo(v, Seq(itemSigner.sign(workflow)))).await(99.s).orThrow
            runOrder(controller, workflow.id, OrderId("A-3"))
          }
        }

        // Recovery
        provider.runController() { controller =>
          controller.httpApi.login_(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))).await(99.s)
          // V2
          // Previously defined workflow is still known
          runOrder(controller, BWorkflowPath ~ V2, OrderId("B-AGAIN"))

          // V4 - Add and use a new workflow
          locally {
            val v = V4
            val workflow = testWorkflow(v) withId CWorkflowPath ~ v
            val signedString = itemSigner.sign(workflow)
            controller.executeCommandAsSystemUser(UpdateRepo(v, Seq(signedString))).await(99.s).orThrow
            controller.runOrder(FreshOrder(OrderId("C"), workflow.path))
          }

          // Change workflow
          provider.updateRepo(controller, V5, testWorkflow(V5).withId(CWorkflowPath) :: Nil)

          // Delete workflow
          provider.updateRepo(controller, V6, delete = CWorkflowPath :: Nil)
          assert(Try { runOrder(controller, CWorkflowPath ~ V6, OrderId("B-6")) }
            .failed.get.asInstanceOf[HttpException].problem contains ItemDeletedProblem(CWorkflowPath))

          // Command is rejected due to duplicate VersionId
          assert(controller.executeCommandAsSystemUser(UpdateRepo(V2)).await(99.s) ==
            Left(DuplicateKey("VersionId", V2)))

          // AWorkflowPath is still version V3
          runOrder(controller, AWorkflowPath ~ V3, OrderId("A-3"))
          runOrder(controller, BWorkflowPath ~ V2, OrderId("B-2"))

          sys.props.get("ControllerRepoTest").foreach(_.split(" +") match {
            case Array(nString) =>
              val (a, b) = nString.span(_ != '/')
              val (n, bundleFactor) = (a.toInt, b.drop(1).toInt)
              testSpeed(controller.localUri, Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD")), n, bundleFactor)

            case Array(nString, uri, userId, password) =>
              // Fails because JS7 does not know our signature !!!
              val (a, b) = nString.span(_ != '/')
              val (n, bundleFactor) = (a.toInt, b.drop(1).toInt)
              testSpeed(Uri(uri), Some(UserId(userId) -> SecretString(password)), n, bundleFactor)

            case _ => sys.error("Invalid number of arguments in property ControllerRepoTest")
          })
        }
      }

      def runOrder(controller: RunningController, workflowId: WorkflowId, orderId: OrderId): Unit = {
        val order = FreshOrder(orderId, workflowId.path)
        controller.httpApi.addOrder(order).await(99.s)
        awaitOrder(controller, orderId, workflowId)
      }

      def awaitOrder(controller: RunningController, orderId: OrderId, workflowId: WorkflowId): Unit = {
        val orderAdded: OrderAdded = controller.eventWatch.await[OrderAdded](_.key == orderId).head.value.event
        assert(orderAdded.workflowId == workflowId)
        val written = controller.eventWatch.await[OrderStdoutWritten](_.key == orderId).head.value.event
        assert(written.chunk contains s"/VERSION-${workflowId.versionId.string}/")
        controller.eventWatch.await[OrderFinished](_.key == orderId)
      }

      def testSpeed(uri: Uri, credentials: Option[UserAndPassword], n: Int, bundleFactor: Int): Unit = {
        val genStopwatch = new Stopwatch
        val commands = generateCommands(n, bundleFactor)
        logInfo(genStopwatch.itemsPerSecondString(n, "objects signed"))
        val exeStopwatch = new Stopwatch
        testSpeedOfCommands(uri, credentials, commands)
        logInfo(exeStopwatch.itemsPerSecondString(n, "objects"))
      }

      def testSpeedOfCommands(uri: Uri, credentials: Option[UserAndPassword], commands: Seq[UpdateRepo]): Unit =
        AkkaHttpControllerApi.separateAkkaResource(uri, credentials)
          .use(controller =>
            controller.login() >>
              Observable
                .fromIterable(commands)
                .mapParallelUnordered(2)(
                  controller.executeCommand(_))
                .completedL)
          .runToFuture
          .await(1.h)

      def generateCommands(n: Int, bundleFactor: Int): Seq[UpdateRepo] = {
        val workflow0 = Workflow.of(Execute(WorkflowJob(TestAgentName, RelativeExecutablePath(s"EXECUTABLE"))))
        val versionCounter = AtomicInt(0)
        Observable.fromIterable(1 to n)
          .bufferTumbling(bundleFactor)
          .mapParallelUnordered(sys.runtime.availableProcessors)(is => Task {
            val v = VersionId(s"SPEED-${versionCounter.incrementAndGet()}")
            val workflows = for (i <- is) yield workflow0.withId(WorkflowPath(s"/WORKFLOW-$i") ~ v)
            UpdateRepo(v, workflows map provider.sign)
          }
        ).toListL.await(99.s)
      }
    }
  }

  private def logInfo(message: String): Unit = {
    logger.info(message)
    info(message)
  }
}

object ControllerRepoTest
{
  private val logger = Logger(getClass)

  private val AWorkflowPath = WorkflowPath("/A")
  private val BWorkflowPath = WorkflowPath("/B")
  private val CWorkflowPath = WorkflowPath("/C")
  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")
  private val V4 = VersionId("4")
  private val V5 = VersionId("5")
  private val V6 = VersionId("6")
  private val TestAgentName = AgentName("AGENT")

  private def testWorkflow(versionId: VersionId) = Workflow.of(
    Execute(WorkflowJob(TestAgentName, RelativeExecutablePath(s"EXECUTABLE-V${versionId.string}$sh"))))
}
