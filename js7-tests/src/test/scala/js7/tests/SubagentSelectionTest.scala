package js7.tests

import cats.syntax.traverse._
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDeleted}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.VersionId
import js7.data.order.OrderEvent.OrderProcessingStarted
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentRefStateEvent.{SubagentCoupled, SubagentCouplingFailed}
import js7.data.subagent.{SubagentId, SubagentRef, SubagentSelection, SubagentSelectionId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.SubagentSelectionTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class SubagentSelectionTest extends AnyFreeSpec
with DirectoryProviderForScalaTest
with SubagentTester
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    js7.auth.subagents.AGENT-1 = "AGENT-PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "AGENT-PASSWORD"
    js7.auth.subagents.C-SUBAGENT = "AGENT-PASSWORD"
    js7.auth.subagents.D-SUBAGENT = "AGENT-PASSWORD"
    """
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Nil

  private lazy val bSubagentRef = newSubagentRef(bSubagentId)
  private lazy val cSubagentRef = newSubagentRef(cSubagentId)
  private lazy val dSubagentRef = newSubagentRef(dSubagentId)
  private lazy val subagentItems = Seq(bSubagentRef, cSubagentRef, dSubagentRef)

  protected implicit val scheduler = Scheduler.global

  private lazy val agent = directoryProvider.startAgent(agentPath).await(99.s)

  override def beforeAll() = {
    super.beforeAll()
    startSubagentTester()
    agent
  }

  override def afterAll() = {
    stopSubagentTester()
    for (a <- Option(agent)) a.terminate().await(99.s)
    super.beforeAll()
  }

  import controller.eventWatch

  private val nextOrderId = Iterator.from(1).map(i => OrderId(s"ORDER-$i")).next _

  private lazy val idToRelease = subagentItems
    .traverse(subagentItem =>
      subagentResource(subagentItem)
        .allocated
        .map(subagentItem.id -> _._2))
    .await(99.s)
    .toMap

  "Start and attach Subagents and SubagentSelection" in {
    // Start Subagents
    idToRelease

    controllerApi
      .updateItems(
        Observable(
          Observable(
            AddOrChangeSimple(subagentSelection),
            AddVersion(versionId),
            AddOrChangeSigned(toSignedString(workflow))),
          Observable
            .fromIterable(subagentItems)
            .map(AddOrChangeSimple(_))
        ).flatten)
      .await(99.s)
      .orThrow

    for (id <- subagentItems.map(_.id)) {
      eventWatch.await[ItemAttached](_.event.key == id)
      eventWatch.await[SubagentCoupled](_.key == id)
    }
  }

  "Orders must distribute on C-SUBAGENT and D-SUBAGENT (priority=2)" in {
    // Subagent usage sequence must be C-SUBAGENT, D-SUBAGENT, C-SUBAGENT
    // because they have the highest priority=2
    runOrdersAndCheck(3, Map(
      cSubagentId -> 2,
      dSubagentId -> 1))
  }

  "Stop D-SUBAGENT: only C-SUBAGENT (prioerity=2) is used" in {
    // After stopping D-SUBAGENT, only C-SUBAGENT has the highest priority=2
    stopSubagentAndRunOrders(dSubagentId, 3, Map(
      cSubagentId -> 3))
  }

  "Stop C-SUBAGENT: only B-SUBAGENT (priority=1) is used" in {
    // After stopping C-SUBAGENT, B-SUBAGENT has the highest priority=1
    stopSubagentAndRunOrders(cSubagentId, 3, Map(
      bSubagentId -> 3))
  }

  def stopSubagentAndRunOrders(stopSubagentId: SubagentId, n: Int, expected: Map[SubagentId, Int])
  : Unit = {
    val eventId = eventWatch.lastAddedEventId
    idToRelease(stopSubagentId).await(99.s)
    eventWatch.await[SubagentCouplingFailed](_.key == stopSubagentId, after = eventId)

    runOrdersAndCheck(n, expected)
  }

  def runOrdersAndCheck(n: Int, expected: Map[SubagentId, Int]): Unit = {
    val eventId = eventWatch.lastAddedEventId
    val orderIds = for (i <- 1 to n) yield nextOrderId()
    controllerApi.addOrders(Observable.fromIterable(orderIds).map(toOrder))
      .await(99.s).orThrow
    val started = for (orderId <- orderIds) yield
      eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
    assert(started.flatMap(_.subagentId).groupMapReduce(identity)(_ => 1)(_ + _) == expected)
  }

  "Stop B-SUBAGENT" in {
    val eventId = eventWatch.lastAddedEventId
    idToRelease(bSubagentId).await(99.s)
    eventWatch.await[SubagentCouplingFailed](_.key == bSubagentId, after = eventId)
  }

  "OrderSelection can only be deleted after Workflow" in {
    val eventId = eventWatch.lastAddedEventId
    val checked = controllerApi
      .updateItems(Observable(DeleteSimple(subagentSelection.id)))
      .await(99.s)
    assert(checked == Left(ItemIsStillReferencedProblem(subagentSelection.id, workflow.id)))

    controllerApi
      .updateItems(Observable(
        AddVersion(VersionId("DELETE")),
        RemoveVersioned(workflow.path)))
      .await(99.s)
      .orThrow
    eventWatch.await[ItemDeleted](_.event.key == workflow.id, after = eventId)
  }

  "Subagent can only be deleted after SubagentSelection ?" in {
    val checked = controllerApi
      .updateItems(Observable(DeleteSimple(dSubagentId)))
      .await(99.s)
    assert(checked == Left(ItemIsStillReferencedProblem(dSubagentId, subagentSelection.id)))
  }

  "Delete OrderSelection" in {
    val eventId = eventWatch.lastAddedEventId
    controllerApi
      .updateItems(Observable(DeleteSimple(subagentSelection.id)))
      .await(99.s)
    eventWatch.await[ItemDeleted](_.event.key == subagentSelection.id, after = eventId)
  }

  "Delete Subagents" in {
    val eventId = eventWatch.lastAddedEventId
    controllerApi
      .updateItems(Observable(
        DeleteSimple(bSubagentId),
        DeleteSimple(cSubagentId),
        DeleteSimple(dSubagentId)))
      .await(99.s).orThrow
    eventWatch.await[ItemDeleted](_.event.key == bSubagentId, after = eventId)
    eventWatch.await[ItemDeleted](_.event.key == cSubagentId, after = eventId)
    eventWatch.await[ItemDeleted](_.event.key == dSubagentId, after = eventId)
  }
}

object SubagentSelectionTest
{
  private val agentPath = AgentPath("AGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")
  private val cSubagentId = SubagentId("C-SUBAGENT")
  private val dSubagentId = SubagentId("D-SUBAGENT")

  private def newSubagentRef(id: SubagentId) =
    SubagentRef(id, agentPath, Uri(s"http://localhost:${findFreeTcpPort()}"))

  private val subagentSelection = SubagentSelection(
    SubagentSelectionId("SELECTION"),
    Map(
      bSubagentId -> 1,
      cSubagentId -> 2,
      dSubagentId -> 2))

  private val versionId = VersionId("VERSION")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ versionId,
    Seq(
      EmptyJob.execute(
        agentPath,
        subagentSelectionId = Some(subagentSelection.id),
        parallelism = 100)))

  private def toOrder(orderId: OrderId) =
    FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
}
