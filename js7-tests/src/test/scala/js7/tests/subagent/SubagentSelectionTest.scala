package js7.tests.subagent

import cats.syntax.traverse.*
import js7.agent.RunningAgent
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.Test
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentCouplingFailed}
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDeleted}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderDeleted, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed}
import js7.data.subagent.{SubagentId, SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentSelectionTest.*
import monix.execution.Scheduler
import monix.reactive.Observable

final class SubagentSelectionTest extends Test with SubagentTester
{
  override protected def agentConfig = config"""
    js7.auth.subagents.A-SUBAGENT = "AGENT-PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "AGENT-PASSWORD"
    js7.auth.subagents.C-SUBAGENT = "AGENT-PASSWORD"
    js7.auth.subagents.D-SUBAGENT = "AGENT-PASSWORD"
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  private lazy val aSubagentItem = newSubagentItem(aSubagentId)
  private lazy val bSubagentItem = newSubagentItem(bSubagentId)
  private lazy val cSubagentItem = newSubagentItem(cSubagentId)
  private lazy val dSubagentItem = newSubagentItem(dSubagentId)
  private lazy val subagentItems = Seq(aSubagentItem, bSubagentItem, cSubagentItem, dSubagentItem)

  private var myAgent: RunningAgent = null

  protected implicit val scheduler = Scheduler.traced

  private val nextOrderId = Iterator.from(1).map(i => OrderId(s"ORDER-$i")).next _

  private lazy val idToRelease = subagentItems
    .traverse(subagentItem =>
      directoryProvider.subagentResource(subagentItem)
        .allocated
        .map(subagentItem.id -> _._2.memoize))
    .await(99.s)
    .toMap

  override def beforeAll() = {
    super.beforeAll()
    myAgent = agent
  }

  override def afterAll() = {
    idToRelease.values.toVector.sequence.await(99.s)
    super.afterAll()
  }

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

  "Recover SubagentSelection when Agent has restarted" in {
    val eventId = eventWatch.lastAddedEventId
    myAgent.terminate().await(99.s)
    eventWatch.await[AgentCouplingFailed](_.key == agentPath, after = eventId)

    myAgent = directoryProvider.startAgent(agentPath).await(99.s)
    eventWatch.await[AgentCoupled](_.key == agentPath, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == aSubagentId, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == bSubagentId, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == cSubagentId, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == dSubagentId, after = eventId)
  }

  "Stop D-SUBAGENT: only C-SUBAGENT (priority=2) is used" in {
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
    val orderIds = Vector.fill(n) { nextOrderId() }
    controllerApi.addOrders(Observable.fromIterable(orderIds).map(toOrder))
      .await(99.s).orThrow
    val started = for (orderId <- orderIds) yield
      eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
    assert(started.flatMap(_.subagentId).groupMapReduce(identity)(_ => 1)(_ + _) == expected)
    for (orderId <- orderIds) eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)
  }

  "Change SubagentSelection" in {
    val eventId = eventWatch.lastAddedEventId
    val changed = subagentSelection.copy(subagentToPriority = Map(
      aSubagentId -> 1))
    controllerApi
      .updateItems(Observable(AddOrChangeSimple(changed)))
      .await(99.s)
      .orThrow

    val orderId = OrderId("CHANGED-SUBAGENTSELECTION")
    controllerApi.addOrder(toOrder(orderId)).await(99.s).orThrow
    val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
      .head.value.event
    assert(started.subagentId.contains(aSubagentId))
    eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)
  }

  "Stop A-SUBAGENT" in {
    val eventId = eventWatch.lastAddedEventId
    idToRelease(aSubagentId).await(99.s)
    eventWatch.await[SubagentCouplingFailed](_.key == aSubagentId, after = eventId)
  }

  "SubagentSelection can only be deleted after Workflow" in {
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

  "Subagent can only be deleted after SubagentSelection" in {
    val checked = controllerApi
      .updateItems(Observable(DeleteSimple(aSubagentId)))
      .await(99.s)
    assert(checked == Left(ItemIsStillReferencedProblem(aSubagentId, subagentSelection.id)))
  }

  "Delete SubagentSelection" in {
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
  private val aSubagentId = SubagentId("A-SUBAGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")
  private val cSubagentId = SubagentId("C-SUBAGENT")
  private val dSubagentId = SubagentId("D-SUBAGENT")

  private def newSubagentItem(id: SubagentId) =
    SubagentItem(id, agentPath, Uri(s"http://localhost:${findFreeTcpPort()}"))

  private val subagentSelection = SubagentSelection(
    SubagentSelectionId("SELECTION"),
    Map(
      aSubagentId -> 1,
      bSubagentId -> 2,
      cSubagentId -> 3,
      dSubagentId -> 3))

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
