package js7.tests.controller

import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.auth.User.UserDoesNotHavePermissionProblem
import js7.base.auth.{UpdateItemPermission, UserId}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.RichFutures
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.system.ServerOperatingSystem.operatingSystem.sleepingShellScript
import js7.data.Problems.{ItemVersionDoesNotMatchProblem, VersionedItemRemovedProblem}
import js7.data.agent.AgentPath
import js7.data.board.{Board, BoardPath}
import js7.data.controller.ControllerCommand.{ControlWorkflow, DeleteOrdersWhenTerminated}
import js7.data.event.{EventRequest, EventSeq}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.job.{JobResource, JobResourcePath, RelativePathExecutable}
import js7.data.lock.{Lock, LockPath}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.{Finish, PostNotices}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowParser, WorkflowPath}
import js7.tests.controller.UpdateItemsTest.*
import js7.tests.testenv.ControllerTestUtils.syntax.RichRunningController
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import scala.concurrent.Promise
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class UpdateItemsTest 
  extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:
  
  protected val agentPaths = agentPath :: Nil
  protected val items = Nil

  override def beforeAll() =
    directoryProvider.controllerEnv.privateConf ++=
       """js7.auth.users {
         |  TEST-USER {
         |    permissions = [ UpdateItem ]
         |  }
         |  without-permission {
         |    password = "plain:TEST-PASSWORD"
         |  }
         |}
         |""".stripMargin
    directoryProvider.agentToEnv(agentPath).writeExecutable(RelativePathExecutable("SCRIPT1.cmd"), sleepingShellScript(2 * Tick))
    directoryProvider.agentToEnv(agentPath).writeExecutable(RelativePathExecutable("SCRIPT2.cmd"), ":")
    directoryProvider.agentToEnv(agentPath).writeExecutable(RelativePathExecutable("SCRIPT4.cmd"), ":")
    super.beforeAll()

  "User requires permission 'UpdateItem'" in:
    val controllerApi = controller.newControllerApi(Some(UserId("without-permission") -> SecretString("TEST-PASSWORD")))
    assert(controllerApi.updateItems(Observable(AddVersion(V1), AddOrChangeSigned(toSignedString(workflow1)))).await(99.s) ==
      Left(UserDoesNotHavePermissionProblem(UserId("without-permission"), UpdateItemPermission)))
    controllerApi.stop.await(99.s)

  "ControllerCommand.UpdateRepo with VersionedItem" in:
    val orderIds = Vector(OrderId("🔺"), OrderId("🔷"))
    controller.api.updateItems(Observable(AddVersion(V1), AddOrChangeSigned(toSignedString(workflow1)))).await(99.s).orThrow
    controller.api.addOrders(Observable(FreshOrder(orderIds(0), workflowPath))).await(99.s).orThrow

    locally:
      val signedWorkflow2 = toSignedString(workflow2)
      controller.api.updateItems(Observable(AddVersion(V2), AddOrChangeSigned(signedWorkflow2))).await(99.s).orThrow
      controller.api.updateItems(Observable(AddVersion(V2), AddOrChangeSigned(signedWorkflow2))).await(99.s).orThrow  /*Duplicate effect is ignored*/
    controller.api.addOrders(Observable(FreshOrder(orderIds(1), workflowPath))).await(99.s).orThrow

    val promises = Vector.fill(2)(Promise[Deadline]())
    for i <- orderIds.indices do
      controller.eventWatch
        .when[OrderFinished](EventRequest.singleClass[OrderFinished](timeout = Some(99.s)), _.key == orderIds(i))
        .foreach:
          case EventSeq.NonEmpty(_) => promises(i).success(now)
          case o => promises(i).failure(new AssertionError(s"Unexpected: $o"))
    val finishedAt = promises.map(_.future) await 99.s
    // The two order running on separate workflow versions run in parallel
    assert(finishedAt(0) > finishedAt(1) + Tick)  // The second added order running on workflow version 2 finished before the first added order

    controller.api.updateItems(Observable(AddVersion(V3), RemoveVersioned(workflowPath))).await(99.s).orThrow
    controller.api.updateItems(Observable(AddVersion(V3), RemoveVersioned(workflowPath))).await(99.s).orThrow  /*Duplicate effect is ignored*/
    assert(controller.api.addOrder(FreshOrder(OrderId("⬛️"), workflowPath)).await(99.s) ==
      Left(VersionedItemRemovedProblem(workflowPath)))

    controller.api.executeCommand(DeleteOrdersWhenTerminated(orderIds)).await(99.s).orThrow

    withClue("Tampered with configuration: "):
      assert(controller.api.updateItems(Observable(
        AddVersion(VersionId("vTampered")),
        AddOrChangeSigned(toSignedString(workflow2).tamper)
      )).await(99.s) == Left(TamperedWithSignedMessageProblem))

  "Divergent VersionId is rejected" in:
    // The signer signs the VersionId, too
    assert(controller.api.updateItems(Observable(
      AddVersion(VersionId("DIVERGE")),
      AddOrChangeSigned(toSignedString(otherWorkflow4))
    )).await(99.s) == Left(ItemVersionDoesNotMatchProblem(VersionId("DIVERGE"), otherWorkflow4.id)))

  "SimpleItem's ItemRevision must not be supplied" in:
    // itemRevision is set only be the Controller
    val lock = Lock(LockPath("LOCK"))
    assert(controller.api.updateUnsignedSimpleItems(Seq(lock.copy(itemRevision = Some(ItemRevision(1))))).await(99.s) ==
      Left(Problem("ItemRevision is not accepted here")))

    controller.api.updateUnsignedSimpleItems(Seq(lock)).await(99.s).orThrow

    assert(controller.api.updateUnsignedSimpleItems(Seq(lock.copy(limit = 7, itemRevision = Some(ItemRevision(1))))).await(99.s) ==
      Left(Problem("ItemRevision is not accepted here")))

  "SignableItem" in:
    controller.api.updateItems(Observable(AddOrChangeSigned(toSignedString(jobResource))))
      .await(99.s).orThrow

  "SignableItem, tampered" in:
    assert(controller.api.updateItems(Observable(AddOrChangeSigned(toSignedString(jobResource).tamper)))
      .await(99.s) == Left(TamperedWithSignedMessageProblem))

  "Change a Workflow and delete the unused Board" in:
    val board = Board.singleNotice(BoardPath("BOARD"))

    val workflow = Workflow(WorkflowPath("WORKFLOW-WITH-BOARD"), Seq(
      PostNotices(Seq(board.path))))

    updateItems(workflow, board)

    val v2 = nextVersionId()
    val workflow2 = Workflow(workflow.path ~ v2, Nil)
    controller.api
      .updateItems(Observable(
        AddVersion(v2),
        AddOrChangeSigned(toSignedString(workflow2)),
        DeleteSimple(board.path)))
      .await(99.s)
      .orThrow

  "Change a Workflow with a WorkflowControl" in:
    val workflow = Workflow(WorkflowPath("WORKFLOW-WITH-CONTROL"), Nil)
    val workflowControl = WorkflowControl(WorkflowControlId(workflow.id))

    val Some(v1) = updateItems(workflow): @unchecked
    assert(controllerState.idToWorkflow.isDefinedAt(workflow.path ~ v1))

    controller.api
      .executeCommand(ControlWorkflow(workflow.path ~ v1, addBreakpoints = Set(Position(0))))
      .await(99.s).orThrow
    assert(controllerState.keyToItem(WorkflowControl).contains(workflowControl.path ~ v1))

    val workflow2 = Workflow(workflow.path, Seq(Finish()))
    val Some(v2) = updateItems(workflow2): @unchecked

    assert(!controllerState.keyToItem(Workflow).contains(workflow.path ~ v1))
    assert(controllerState.keyToItem(Workflow).contains(workflow.path ~ v2))
    assert(!controllerState.keyToItem(WorkflowControl).contains(workflowControl.path ~ v1))
    assert(!controllerState.keyToItem(WorkflowControl).contains(workflowControl.path ~ v2))


object UpdateItemsTest:
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
