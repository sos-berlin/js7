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
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.RemoveOrdersWhenTerminated
import js7.data.event.{EventRequest, EventSeq}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, DeleteVersioned}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.job.{JobResource, JobResourcePath, RelativePathExecutable}
import js7.data.lock.{Lock, LockPath}
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
  protected val agentPaths = agentPath :: Nil
  protected val items = Nil

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
    directoryProvider.agentToTree(agentPath).writeExecutable(RelativePathExecutable("SCRIPT1.cmd"), sleepingShellScript(2 * Tick))
    directoryProvider.agentToTree(agentPath).writeExecutable(RelativePathExecutable("SCRIPT2.cmd"), ":")
    directoryProvider.agentToTree(agentPath).writeExecutable(RelativePathExecutable("SCRIPT4.cmd"), ":")
    super.beforeAll()
  }

  "User requires permission 'UpdateItem'" in {
    val controllerApi = controller.newControllerApi(Some(UserId("without-permission") -> SecretString("TEST-PASSWORD")))
    assert(controllerApi.updateItems(Observable(AddVersion(V1), AddOrChangeSigned(toSignedString(workflow1)))).await(99.s) ==
      Left(UserDoesNotHavePermissionProblem(UserId("without-permission"), UpdateItemPermission)))

    controller.httpApi.login_(Some(directoryProvider.controller.userAndPassword)) await 99.s
  }

  "ControllerCommand.UpdateRepo with VersionedItem" in {
    val orderIds = Vector(OrderId("🔺"), OrderId("🔵"))
    controllerApi.updateItems(Observable(AddVersion(V1), AddOrChangeSigned(toSignedString(workflow1)))).await(99.s).orThrow
    controllerApi.addOrders(Observable(FreshOrder(orderIds(0), workflowPath))).await(99.s).orThrow

    locally {
      val signedWorkflow2 = toSignedString(workflow2)
      controllerApi.updateItems(Observable(AddVersion(V2), AddOrChangeSigned(signedWorkflow2))).await(99.s).orThrow
      controllerApi.updateItems(Observable(AddVersion(V2), AddOrChangeSigned(signedWorkflow2))).await(99.s).orThrow  /*Duplicate effect is ignored*/
    }
    controllerApi.addOrders(Observable(FreshOrder(orderIds(1), workflowPath))).await(99.s).orThrow

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
    controllerApi.executeCommand(RemoveOrdersWhenTerminated(orderIds)).await(99.s).orThrow

    controllerApi.updateItems(Observable(AddVersion(V3), DeleteVersioned(workflowPath))).await(99.s).orThrow
    controllerApi.updateItems(Observable(AddVersion(V3), DeleteVersioned(workflowPath))).await(99.s).orThrow  /*Duplicate effect is ignored*/
    assert(controllerApi.addOrder(FreshOrder(orderIds(1), workflowPath)).await(99.s) ==
      Left(VersionedItemDeletedProblem(workflowPath)))

    withClue("Tampered with configuration: ") {
      assert(controllerApi.updateItems(Observable(
        AddVersion(VersionId("vTampered")),
        AddOrChangeSigned(toSignedString(workflow2).tamper)
      )).await(99.s) == Left(TamperedWithSignedMessageProblem))
    }
  }

  "Divergent VersionId is rejected" in {
    // The signer signs the VersionId, too
    assert(controllerApi.updateItems(Observable(
      AddVersion(VersionId("DIVERGE")),
      AddOrChangeSigned(toSignedString(otherWorkflow4))
    )).await(99.s) == Left(ItemVersionDoesNotMatchProblem(VersionId("DIVERGE"), otherWorkflow4.id)))
  }

  "SimpleItem's ItemRevision must not be supplied" in {
    // itemRevision is set only be the Controller
    val lock = Lock(LockPath("LOCK"))
    assert(controllerApi.updateUnsignedSimpleItems(Seq(lock.copy(itemRevision = Some(ItemRevision(1))))).await(99.s) ==
      Left(Problem("ItemRevision is not accepted here")))

    controllerApi.updateUnsignedSimpleItems(Seq(lock)).await(99.s).orThrow

    assert(controllerApi.updateUnsignedSimpleItems(Seq(lock.copy(limit = 7, itemRevision = Some(ItemRevision(1))))).await(99.s) ==
      Left(Problem("ItemRevision is not accepted here")))
  }

  "SignableItem" in {
    controllerApi.updateItems(Observable(AddOrChangeSigned(toSignedString(jobResource))))
      .await(99.s).orThrow
  }

  "SignableItem, tampered" in {
    assert(controllerApi.updateItems(Observable(AddOrChangeSigned(toSignedString(jobResource).tamper)))
      .await(99.s) == Left(TamperedWithSignedMessageProblem))
  }
}

object UpdateItemsTest
{
  private val Tick = 2.s
  private val agentPath = AgentPath("AGENT")

  private val workflowPath = WorkflowPath("WORKFLOW")
  private val script1 = """
    define workflow {
      execute executable="SCRIPT1.cmd", agent="AGENT";
    }"""

  private val V1 = VersionId("1")
  private val workflow1 = WorkflowParser.parse(workflowPath ~ V1, script1).orThrow

  private val V2 = VersionId("2")
  private val script2 = """
    define workflow {
      execute executable="SCRIPT2.cmd", agent="AGENT";
    }"""
  private val workflow2 = WorkflowParser.parse(workflowPath ~ V2, script2).orThrow

  private val V3 = VersionId("3")

  private val V4 = VersionId("5")
  private val script4 = """
    define workflow {
      execute executable="SCRIPT4.cmd", agent="AGENT";
    }"""
  private val otherWorkflow4 = WorkflowParser.parse(WorkflowPath("OTHER-WORKFLOW") ~ V4, script4).orThrow

  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
}
