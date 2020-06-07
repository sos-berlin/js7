package js7.tests

import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.data.agent.AgentRefPath
import js7.data.filebased.Repo.FileBasedDeletedProblem
import js7.data.filebased.VersionId
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import js7.master.RunningMaster
import js7.master.client.{AkkaHttpMasterApi, MasterApi}
import js7.master.data.MasterCommand.UpdateRepo
import js7.tests.testenv.DirectoryProvider
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Try

final class MasterRepoTest extends AnyFreeSpec
{
  import MasterRepoTest._

  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentRefPath), testName = Some("MasterRepoTest"))) { provider =>
      for (v <- 1 to 4)  // For each version, we use a dedicated job which echos the VersionId
        provider.agents.head.writeExecutable(ExecutablePath(s"/EXECUTABLE-V$v$sh"), (if (isWindows) "@" else "") + s"echo /VERSION-$v/")
      provider.master.configDir / "master.conf" ++=
        """js7.auth.users.TEST-USER {
          |  password = "plain:TEST-PASSWORD"
          |  permissions = [ UpdateRepo ]
          |}
          |""".stripMargin

      provider.runAgents() { _ =>
        provider.runMaster() { master =>
          master.httpApi.login_(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))).await(99.s)
          // Add Workflow
          addWorkflowAndRunOrder(master, V1, AWorkflowPath, OrderId("A"))

          // Command is rejected due to duplicate VersionId
          assert(master.executeCommandAsSystemUser(UpdateRepo(V1)).await(99.s) ==
            Left(DuplicateKey("VersionId", V1)))

          // Add Workflow
          addWorkflowAndRunOrder(master, V2, BWorkflowPath, OrderId("B"))

          // Change Workflow
          changeWorkflowAndRunOrder(master, V3, AWorkflowPath, OrderId("A-3"))
        }
        // Recovery
        provider.runMaster() { master =>
          master.httpApi.login_(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))).await(99.s)
          // V2
          // Previously defined workflow is still known
          runOrder(master, BWorkflowPath ~ V2, OrderId("B-AGAIN"))

          // V4 - Add and use a new workflow
          addWorkflowAndRunOrder(master, V4, CWorkflowPath, OrderId("C"))

          // Change workflow
          provider.updateRepo(master, V5, testWorkflow(V5).withId(CWorkflowPath) :: Nil)

          // Delete workflow
          provider.updateRepo(master, V6, delete = CWorkflowPath :: Nil)
          assert(Try { runOrder(master, CWorkflowPath ~ V6, OrderId("B-6")) }
            .failed.get.asInstanceOf[HttpException].problem contains FileBasedDeletedProblem(CWorkflowPath ~ V6))

          // Command is rejected due to duplicate VersionId
          assert(master.executeCommandAsSystemUser(UpdateRepo(V2)).await(99.s) ==
            Left(DuplicateKey("VersionId", V2)))

          // AWorkflowPath is still version V3
          runOrder(master, AWorkflowPath ~ V3, OrderId("A-3"))
          runOrder(master, BWorkflowPath ~ V2, OrderId("B-2"))

          sys.props.get("MasterRepoTest").foreach(_.split(" +") match {
            case Array(nString) =>
              master.httpApi.login_(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))).await(99.s)
              testSpeed(master.httpApi, n = nString.toInt)

            case Array(nString, uri, userId, password) =>
              // Fails because JS7 does not know our signature !!!
              AkkaHttpMasterApi.separateAkkaResource(Uri(uri), Some(UserId(userId) -> SecretString(password)))
                .use(masterApi =>
                  masterApi.login() >>
                    Task {
                      testSpeed(masterApi, n = nString.toInt)
                    })
                .runToFuture
                .await(1.h)

            case _ => sys.error("Invalid number of arguments in property MasterRepoTest")
          })
        }
      }

      def addWorkflowAndRunOrder(master: RunningMaster, versionId: VersionId, path: WorkflowPath, orderId: OrderId): Unit = {
        val order = FreshOrder(orderId, path)
        // Command will be rejected because workflow is not yet defined
        assert(master.addOrder(order).runToFuture.await(99.s) == Left(UnknownKeyProblem("TypedPath", path)))
        defineWorkflowAndRunOrder(master, versionId, path, orderId)
      }

      def changeWorkflowAndRunOrder(master: RunningMaster, versionId: VersionId, path: WorkflowPath, orderId: OrderId): Unit =
        defineWorkflowAndRunOrder(master, versionId, path, orderId)

      def defineWorkflowAndRunOrder(master: RunningMaster, versionId: VersionId, path: WorkflowPath, orderId: OrderId): Unit = {
        val workflow = testWorkflow(versionId)
        assert(workflow.isAnonymous)
        val order = FreshOrder(orderId, path)
        // Add Workflow
        provider.updateRepo(master, versionId, workflow.withId(path) :: Nil)
        master.httpApi.addOrders(order :: Nil).await(99.s)
        awaitOrder(master, order.id, path ~ versionId)
      }

      def runOrder(master: RunningMaster, workflowId: WorkflowId, orderId: OrderId): Unit = {
        val order = FreshOrder(orderId, workflowId.path)
        master.httpApi.addOrder(order).await(99.s)
        awaitOrder(master, orderId, workflowId)
      }

      def awaitOrder(master: RunningMaster, orderId: OrderId, workflowId: WorkflowId): Unit = {
        val orderAdded: OrderAdded = master.eventWatch.await[OrderAdded](_.key == orderId).head.value.event
        assert(orderAdded.workflowId == workflowId)
        val written = master.eventWatch.await[OrderStdoutWritten](_.key == orderId).head.value.event
        assert(written.chunk contains s"/VERSION-${workflowId.versionId.string}/")
        master.eventWatch.await[OrderFinished](_.key == orderId)
      }

      def testSpeed(masterApi: MasterApi, n: Int): Unit = {
        val workflow0 = testWorkflow(V1)
        val stopwatch = new Stopwatch
        Observable
          .fromIterable(1 to n)
          .mapParallelUnordered(parallelism = (sys.runtime.availableProcessors / 2) max 1) { i =>
            val v = VersionId(s"SPEED-$i")
            val workflow = workflow0.withId(WorkflowPath(s"/WORKFLOW-$i") ~ VersionId(s"SPEED-$i"))
            masterApi.executeCommand(UpdateRepo(v, provider.sign(workflow) :: Nil))
          }
          .completedL
          .runToFuture
          .await(1.h)
        val stopwatchResult = stopwatch.itemsPerSecondString(n, "UpdateRepos")
        logger.info(stopwatchResult)
        info(stopwatchResult)
      }
    }
  }
}

object MasterRepoTest
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
  private val TestAgentRefPath = AgentRefPath("/AGENT")

  private def testWorkflow(versionId: VersionId) = Workflow.of(
    Execute(WorkflowJob(TestAgentRefPath, ExecutablePath(s"/EXECUTABLE-V${versionId.string}$sh"))))
}
