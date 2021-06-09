package js7.tests

import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.process.Processes.{ShellFileExtension => sh}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked._
import js7.base.problem.Problems.DuplicateKey
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.akkautils.Akkas.actorSystemResource
import js7.common.http.AkkaHttpClient.HttpException
import js7.controller.RunningController
import js7.controller.client.AkkaHttpControllerApi.admissionToApiResource
import js7.data.Problems.VersionedItemDeletedProblem
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, TakeSnapshot}
import js7.data.item.BasicItemEvent.ItemDestroyed
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, DeleteVersioned}
import js7.data.item.{ItemOperation, VersionId}
import js7.data.job.{RelativePathExecutable, ScriptExecutable}
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderPrompted, OrderRemoved, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Prompt}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import js7.proxy.ControllerApi
import js7.tests.testenv.ControllerTestUtils.syntax._
import js7.tests.testenv.DirectoryProvider
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.util.Try

final class ControllerRepoTest extends AnyFreeSpec
{
  import ControllerRepoTest._

  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath), testName = Some("ControllerRepoTest"))) { provider =>
      import provider.itemSigner

      for (v <- 1 to 4)  // For each version, we use a dedicated job which echos the VersionId
        provider.agents.head.writeExecutable(RelativePathExecutable(s"EXECUTABLE-V$v$sh"), (isWindows ?? "@") + s"echo /VERSION-$v/")
      provider.controller.configDir / "controller.conf" ++=
        """js7.auth.users.TEST-USER {
          |  password = "plain:TEST-PASSWORD"
          |  permissions = [ UpdateItem ]
          |}
          |""".stripMargin

      val versionCounter = AtomicInt(0)

      provider.runAgents() { _ =>
        provider.runController() { controller =>
          controller.httpApi.login_(Some(userAndPassword)).await(99.s)
          val controllerApi = controller.newControllerApi(Some(userAndPassword))

          withClue("Add Workflow: ") {
            val v = V1
            val workflow = testWorkflow(v) withId AWorkflowPath ~ v
            val signed = itemSigner.sign(workflow)
            controllerApi.updateRepo(v, Seq(signed)).await(99.s).orThrow
            controller.runOrder(FreshOrder(OrderId("A"), workflow.path))

            // Non-empty UpdateRepo with same resulting Repo is accepted
            controllerApi.updateRepo(v, Seq(signed)).await(99.s).orThrow
            controllerApi.updateRepo(v, Seq(signed)).await(99.s).orThrow

            // Empty UpdateRepo with same VersionId is rejected due to duplicate VersionId
            assert(controllerApi.updateRepo(v, Nil).await(99.s) ==
              Left(DuplicateKey("VersionId", v)))
          }

          withClue("Add another Workflow: ") {
            val v = V2
            val workflow = testWorkflow(v) withId BWorkflowPath ~ v
            val signed = itemSigner.sign(workflow)
            controllerApi.updateRepo(v, Seq(signed)).await(99.s).orThrow
            controller.runOrder(FreshOrder(OrderId("B"), workflow.path))
          }

          withClue("Change first Workflow: ") {
            val v = V3
            val workflow = testWorkflow(v) withId AWorkflowPath ~ v
            controllerApi.updateRepo(v, Seq(itemSigner.sign(workflow))).await(99.s).orThrow
            runOrder(controller, workflow.id, OrderId("A-3"))
          }

          withClue("Delete a workflow containing orders: ") {
            val v = VersionId("WITH-ORDER")
            val workflow = Workflow(WorkflowPath("WITH-ORDER") ~ v, Seq(Prompt(StringConstant(""))))
            controllerApi.updateRepo(v, Seq(itemSigner.sign(workflow)))
              .await(99.s).orThrow

            val orderId = OrderId("DELETE-WITH-ORDER")
            controllerApi.addOrder(FreshOrder(orderId, workflow.path), remove = true)
              .await(99.s).orThrow
            controller.eventWatch.await[OrderPrompted](_.key == orderId)

            controllerApi.updateRepo(VersionId("WITH-ORDER-DELETED"), delete = Seq(workflow.path))
              .await(99.s).orThrow

            controller.executeCommandAsSystemUser(AnswerOrderPrompt(orderId))
              .await(99.s).orThrow
            controller.eventWatch.await[OrderRemoved](_.key == orderId)
            controller.eventWatch.await[ItemDestroyed](_.event.key == workflow.id)
          }
        }

        // Recovery
        provider.runController() { controller =>
          controller.httpApi.login_(Some(userAndPassword)).await(99.s)
          val controllerApi = controller.newControllerApi(Some(userAndPassword))

          // V2
          // Previously defined workflow is still known
          runOrder(controller, BWorkflowPath ~ V2, OrderId("B-AGAIN"))

          // V4 - Add and use a new workflow
          locally {
            val v = V4
            val workflow = testWorkflow(v) withId CWorkflowPath ~ v
            val signed = itemSigner.sign(workflow)
            controllerApi.updateRepo(v, Seq(signed)).await(99.s).orThrow
            controller.runOrder(FreshOrder(OrderId("C"), workflow.path))
          }

          // Change workflow
          provider.updateVersionedItems(controller, V5, testWorkflow(V5).withId(CWorkflowPath) :: Nil)

          // Delete workflow
          provider.updateVersionedItems(controller, V6, delete = CWorkflowPath :: Nil)
          assert(Try { runOrder(controller, CWorkflowPath ~ V6, OrderId("B-6")) }
            .failed.get.asInstanceOf[HttpException].problem contains VersionedItemDeletedProblem(CWorkflowPath))

          // Command is rejected due to duplicate VersionId
          assert(controllerApi.updateRepo(V2, Nil).await(99.s) ==
            Left(DuplicateKey("VersionId", V2)))

          // AWorkflowPath is still version V3
          runOrder(controller, AWorkflowPath ~ V3, OrderId("A-3"))
          runOrder(controller, BWorkflowPath ~ V2, OrderId("B-2"))

          sys.props.get("test.speed").foreach(_.split(" +") match {
            case Array(nString) =>
              val (a, b) = nString.span(_ != '*')
              val (n, itemCount) = (a.toInt, b.drop(1).toInt)
              testSpeed(controller.localUri, Some(userAndPassword), n, itemCount)

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

      def testSpeed(uri: Uri, credentials: Option[UserAndPassword], n: Int, itemCount: Int): Unit = {
        val genStopwatch = new Stopwatch
        val operations = generateAddItemOperations(itemCount)
        logInfo(genStopwatch.itemsPerSecondString(itemCount, "items signed"))
        actorSystemResource(name = "ControllerRepoTest-SPEED")
          .use(actorSystem => Task {
            val apiResource  = admissionToApiResource(Admission(uri, credentials))(actorSystem)
            val controllerApi = new ControllerApi(Seq(apiResource))
            for (_ <- 1 to n) {
              val t = now
              controllerApi.updateItems(Observable.fromIterable(operations))
                .runToFuture
                .await(99.s)
                .orThrow
              logInfo(itemsPerSecondString(t.elapsed, itemCount, "items"))
            }
            locally {
              val t = now
              controllerApi.executeCommand(TakeSnapshot)
                .runToFuture
                .await(99.s)
                .orThrow
              logInfo(s"Snapshot taken in ${t.elapsed.pretty}")
            }
            locally {
              val t = now
              controllerApi.updateItems(deleteItemOperations(itemCount))
                .runToFuture
                .await(99.s)
                .orThrow
              logInfo(itemsPerSecondString(t.elapsed, itemCount, "deletions"))
            }

          })
          .runToFuture
          .await(1.h)
      }

      def generateAddItemOperations(n: Int): Seq[ItemOperation] = {
        val workflow0 = Workflow.of(Execute(WorkflowJob(TestAgentPath, ScriptExecutable("# " + "BIG "*256))))
        val v = VersionId(s"SPEED-${versionCounter.incrementAndGet()}")
        Observable.fromIterable(1 to n)
          .mapParallelUnorderedBatch() { i =>
            val workflow = workflow0.withId(WorkflowPath(s"WORKFLOW-$i") ~ v)
            AddOrChangeSigned(provider.toSignedString(workflow))
          }
          .prepend(AddVersion(v))
          .toL(Vector)
          .await(99.s)
      }

      def deleteItemOperations(n: Int): Observable[ItemOperation] = {
        val v = VersionId(s"SPEED-${versionCounter.incrementAndGet()}")
        (Observable(AddVersion(v)) ++
          Observable.fromIterable(1 to n)
            .map(i => DeleteVersioned(WorkflowPath(s"WORKFLOW-$i"))))
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

  private val userAndPassword = UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))
  private val AWorkflowPath = WorkflowPath("A")
  private val BWorkflowPath = WorkflowPath("B")
  private val CWorkflowPath = WorkflowPath("C")
  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")
  private val V4 = VersionId("4")
  private val V5 = VersionId("5")
  private val V6 = VersionId("6")
  private val TestAgentPath = AgentPath("AGENT")

  private def testWorkflow(versionId: VersionId) = Workflow.of(
    Execute(WorkflowJob(TestAgentPath, RelativePathExecutable(s"EXECUTABLE-V${versionId.string}$sh"))))
}
