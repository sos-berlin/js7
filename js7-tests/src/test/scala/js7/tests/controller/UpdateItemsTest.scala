package js7.tests.controller

import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.auth.User.UserDoesNotHavePermissionProblem
import js7.base.auth.{UpdateItemPermission, UserId}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.thread.Futures.implicits.RichFutures
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.common.system.ServerOperatingSystem.operatingSystem.sleepingShellScript
import js7.data.Problems.{ItemVersionDoesNotMatchProblem, VersionedItemDeletedProblem}
import js7.data.agent.AgentId
import js7.data.controller.ControllerCommand.RemoveOrdersWhenTerminated
import js7.data.event.{EventRequest, EventSeq}
import js7.data.item.ItemOperation.{AddVersion, VersionedAddOrChange, VersionedDelete}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.job.RelativePathExecutable
import js7.data.lock.{Lock, LockId}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.controller.UpdateItemsTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.ControllerTestUtils.syntax.RichRunningController
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class UpdateItemsTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = TestAgentId :: Nil
  protected val versionedItems = Nil

  override def beforeAll() = {
    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
       """js7.auth.users {
         |  TEST-USER {
         |    permissions = [ UpdateItem ]
         |  }
         |  without-permission {
         |    password = "plain:TEST-PASSWORD"
         |  }
         |}
         |""".stripMargin
    directoryProvider.agentToTree(TestAgentId).writeExecutable(RelativePathExecutable("SCRIPT1.cmd"), sleepingShellScript(2 * Tick))
    directoryProvider.agentToTree(TestAgentId).writeExecutable(RelativePathExecutable("SCRIPT2.cmd"), ":")
    directoryProvider.agentToTree(TestAgentId).writeExecutable(RelativePathExecutable("SCRIPT4.cmd"), ":")
    super.beforeAll()
  }

  "User requires permission 'UpdateItem'" in {
    val controllerApi = controller.newControllerApi(Some(UserId("without-permission") -> SecretString("TEST-PASSWORD")))
    assert(controllerApi.updateItems(Observable(AddVersion(V1), VersionedAddOrChange(sign(workflow1)))).await(99.s) ==
      Left(UserDoesNotHavePermissionProblem(UserId("without-permission"), UpdateItemPermission)))

    controller.httpApi.login_(Some(directoryProvider.controller.userAndPassword)) await 99.s
  }

  "ControllerCommand.UpdateRepo with VersionedItem" in {
    val orderIds = Vector(OrderId("🔺"), OrderId("🔵"))
    controllerApi.updateItems(Observable(AddVersion(V1), VersionedAddOrChange(sign(workflow1)))).await(99.s).orThrow
    controller.addOrderBlocking(FreshOrder(orderIds(0), TestWorkflowPath))

    locally {
      val signedWorkflow2 = sign(workflow2)
      controllerApi.updateItems(Observable(AddVersion(V2), VersionedAddOrChange(signedWorkflow2))).await(99.s).orThrow
      controllerApi.updateItems(Observable(AddVersion(V2), VersionedAddOrChange(signedWorkflow2))).await(99.s).orThrow  /*Duplicate effect is ignored*/
    }
    controller.addOrderBlocking(FreshOrder(orderIds(1), TestWorkflowPath))

    val promises = Vector.fill(2)(Promise[Deadline]())
    for (i <- orderIds.indices) {
      controller.eventWatch.when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = Some(99.s)), _.key == orderIds(i)) foreach {
        case EventSeq.NonEmpty(_) => promises(i).success(now)
        case o => fail(s"Unexpected: $o")
      }
    }
    val finishedAt = promises.map(_.future) await 99.s
    // The two order running on separate workflow versions run in parallel
    assert(finishedAt(0) > finishedAt(1) + Tick)  // The second added order running on workflow version 2 finished before the first added order
    controller.executeCommandAsSystemUser(RemoveOrdersWhenTerminated(orderIds)).await(99.s).orThrow

    controllerApi.updateItems(Observable(AddVersion(V3), VersionedDelete(TestWorkflowPath))).await(99.s).orThrow
    controllerApi.updateItems(Observable(AddVersion(V3), VersionedDelete(TestWorkflowPath))).await(99.s).orThrow  /*Duplicate effect is ignored*/
    assert(controllerApi.addOrder(FreshOrder(orderIds(1), TestWorkflowPath)).await(99.s) ==
      Left(VersionedItemDeletedProblem(TestWorkflowPath)))

    withClue("Tampered with configuration: ") {
      assert(controllerApi.updateItems(Observable(
        AddVersion(VersionId("vTampered")),
        VersionedAddOrChange(sign(workflow2).copy(string = "TAMPERED"))
      )).await(99.s) == Left(TamperedWithSignedMessageProblem))
    }
  }

  "SimpleItem's ItemRevision must not be supplied" in {
    // itemRevision is set only be the Controller
    val lock = Lock(LockId("LOCK"))
    assert(controllerApi.updateSimpleItems(Seq(lock.copy(itemRevision = Some(ItemRevision(1))))).await(99.s) ==
      Left(Problem("ItemRevision is not accepted here")))

    controllerApi.updateSimpleItems(Seq(lock)).await(99.s).orThrow

    assert(controllerApi.updateSimpleItems(Seq(lock.copy(limit = 7, itemRevision = Some(ItemRevision(1))))).await(99.s) ==
      Left(Problem("ItemRevision is not accepted here")))
  }

  "Divergent VersionId is rejected" in {
    // The signer signs the VersionId, too
    assert(controllerApi.updateItems(Observable(
      AddVersion(VersionId("DIVERGE")),
      VersionedAddOrChange(sign(otherWorkflow5))
    )).await(99.s) == Left(ItemVersionDoesNotMatchProblem(VersionId("DIVERGE"), otherWorkflow5.id)))
  }
}

object UpdateItemsTest
{
  private val Tick = 2.s
  private val TestAgentId = AgentId("AGENT")
  private val TestWorkflowPath = WorkflowPath("WORKFLOW")
  private val script1 = """
    define workflow {
      execute executable="SCRIPT1.cmd", agent="AGENT";
    }"""

  private val V1 = VersionId("1")
  private val workflow1 = WorkflowParser.parse(TestWorkflowPath ~ V1, script1).orThrow

  private val V2 = VersionId("2")
  private val script2 = """
    define workflow {
      execute executable="SCRIPT2.cmd", agent="AGENT";
    }"""
  private val workflow2 = WorkflowParser.parse(TestWorkflowPath ~ V2, script2).orThrow

  private val V3 = VersionId("3")

  private val V4 = VersionId("4")
  private val workflow4 = WorkflowParser.parse(TestWorkflowPath ~ V4, script2).orThrow
  private val otherWorkflow4 = WorkflowParser.parse(WorkflowPath("OTHER-WORKFLOW") ~ V4, script2).orThrow

  private val V5 = VersionId("5")
  private val script5 = """
    define workflow {
      execute executable="SCRIPT4.cmd", agent="AGENT";
    }"""
  private val otherWorkflow5 = WorkflowParser.parse(WorkflowPath("OTHER-WORKFLOW") ~ V5, script5).orThrow
}
