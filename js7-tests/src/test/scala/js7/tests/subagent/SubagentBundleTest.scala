package js7.tests.subagent

import cats.syntax.traverse.*
import fs2.Stream
import js7.agent.TestAgent
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentRefStateEvent.AgentCouplingFailed
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDeleted}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderDeleted, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.expression.Expression.{NumericConstant, StringConstant}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentBundleTest.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class SubagentBundleTest extends OurTestSuite, SubagentTester:

  override protected def agentConfig = config"""
    js7.auth.subagents.A-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.C-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.D-SUBAGENT = "$localSubagentId's PASSWORD"
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  private lazy val aSubagentItem = newSubagentItem(aSubagentId)
  private lazy val bSubagentItem = newSubagentItem(bSubagentId)
  private lazy val cSubagentItem = newSubagentItem(cSubagentId)
  private lazy val dSubagentItem = newSubagentItem(dSubagentId)
  private lazy val subagentItems = Seq(aSubagentItem, bSubagentItem, cSubagentItem, dSubagentItem)

  private var myAgent: TestAgent = null.asInstanceOf[TestAgent]

  private val nextOrderId: () => OrderId =
    Iterator.from(1).map(i => OrderId(s"ORDER-$i")).next

  private lazy val idToRelease = subagentItems
    .traverse(subagentItem =>
      directoryProvider.bareSubagentResource(subagentItem)
        .allocated
        .map(subagentItem.id -> _._2.unsafeMemoize))
    .await(99.s)
    .toMap

  override def beforeAll() =
    super.beforeAll()
    myAgent = agent

  override def afterAll() =
    try
      idToRelease.values.toVector.sequence.await(99.s)
      myAgent.terminate().await(99.s)
    finally
      super.afterAll()

  "Start and attach Subagents and SubagentBundle" in:
    // Start Subagents
    idToRelease

    controller.api
      .updateItems(
        Stream(
          Stream(
            AddOrChangeSimple(subagentBundle),
            AddVersion(versionId),
            AddOrChangeSigned(toSignedString(workflow))),
          Stream
            .iterable(subagentItems)
            .map(AddOrChangeSimple(_))
        ).flatten)
      .await(99.s)
      .orThrow

    for id <- subagentItems.map(_.id) do
      eventWatch.await[ItemAttached](_.event.key == id)
      eventWatch.await[SubagentCoupled](_.key == id)

  "Orders must distribute on C-SUBAGENT and D-SUBAGENT (priority=2)" in:
    // Subagent usage sequence must be C-SUBAGENT, D-SUBAGENT, C-SUBAGENT
    // because they have the highest priority=2
    runOrdersAndCheck(3, Map(
      cSubagentId -> 2,
      dSubagentId -> 1))

  "Recover SubagentBundle when Agent has restarted" in:
    val eventId = eventWatch.lastAddedEventId
    myAgent.terminate().await(99.s)
    eventWatch.await[AgentCouplingFailed](_.key == agentPath, after = eventId)

    myAgent = directoryProvider.startAgent(agentPath).await(99.s)
    //eventWatch.await[AgentCoupled](_.key == agentPath, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == aSubagentId, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == bSubagentId, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == cSubagentId, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == dSubagentId, after = eventId)

  "Stop D-SUBAGENT: only C-SUBAGENT (priority=2) is used" in:
    // After stopping D-SUBAGENT, only C-SUBAGENT has the highest priority=2
    stopSubagentAndRunOrders(dSubagentId, 3, Map(
      cSubagentId -> 3))

  "Stop C-SUBAGENT: only B-SUBAGENT (priority=1) is used" in:
    // After stopping C-SUBAGENT, B-SUBAGENT has the highest priority=1
    stopSubagentAndRunOrders(cSubagentId, 3, Map(
      bSubagentId -> 3))

  def stopSubagentAndRunOrders(stopSubagentId: SubagentId, n: Int, expected: Map[SubagentId, Int])
  : Unit =
    val eventId = eventWatch.lastAddedEventId
    idToRelease(stopSubagentId).await(99.s)
    eventWatch.await[SubagentCouplingFailed](_.key == stopSubagentId, after = eventId)

    runOrdersAndCheck(n, expected)

  def runOrdersAndCheck(n: Int, expected: Map[SubagentId, Int]): Unit =
    val eventId = eventWatch.lastAddedEventId
    val orderIds = Vector.fill(n) { nextOrderId() }
    controller.api.addOrders(Stream.iterable(orderIds).map(toOrder))
      .await(99.s).orThrow
    val started = for orderId <- orderIds yield
      eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
    assert(started.flatMap(_.subagentId).groupMapReduce(identity)(_ => 1)(_ + _) == expected)
    for orderId <- orderIds do eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)

  "Change SubagentBundle" in:
    val eventId = eventWatch.lastAddedEventId
    val changed = subagentBundle.copy(subagentToPriority = Map(
      aSubagentId -> NumericConstant(1)))
    controller.api
      .updateItems(Stream(AddOrChangeSimple(changed)))
      .await(99.s)
      .orThrow

    val orderId = OrderId("CHANGED-SUBAGENTBUNDLE")
    controller.api.addOrder(toOrder(orderId)).await(99.s).orThrow
    val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
      .head.value.event
    assert(started.subagentId.contains(aSubagentId))
    eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)

  "Use SubagentId as SubagentBundleId" in:
    val workflow = updateItem(Workflow(
      WorkflowPath("SUBAGENT-ID-AS-BUNDLE"),
      Seq(
        EmptyJob.execute(
          agentPath,
          subagentBundleId = Some(StringConstant(bSubagentId.string))))))
    val events = controller.runOrder(FreshOrder(OrderId("SUBAGENT-ID-AS-BUNDLE"), workflow.path))
    assert(events.map(_.value) contains OrderProcessingStarted(bSubagentId))

  "Stop A-SUBAGENT" in:
    val eventId = eventWatch.lastAddedEventId
    idToRelease(aSubagentId).await(99.s)
    eventWatch.await[SubagentCouplingFailed](_.key == aSubagentId, after = eventId)

  "SubagentBundle can only be deleted after Workflow" in:
    val eventId = eventWatch.lastAddedEventId
    val checked = controller.api
      .updateItems(Stream(DeleteSimple(subagentBundle.id)))
      .await(99.s)
    assert(checked == Left(ItemIsStillReferencedProblem(subagentBundle.id, workflow.id,
      moreInfo = ", attached to Agent:AGENT")))

    controller.api
      .updateItems(Stream(
        AddVersion(VersionId("DELETE")),
        RemoveVersioned(workflow.path)))
      .await(99.s)
      .orThrow
    eventWatch.await[ItemDeleted](_.event.key == workflow.id, after = eventId)

  "Subagent can only be deleted after SubagentBundle" in:
    val checked = controller.api
      .updateItems(Stream(DeleteSimple(aSubagentId)))
      .await(99.s)
    assert(checked == Left(ItemIsStillReferencedProblem(aSubagentId, subagentBundle.id,
      moreInfo = ", attached to Agent:AGENT")))

  "Delete SubagentBundle" in:
    val eventId = eventWatch.lastAddedEventId
    controller.api
      .updateItems(Stream(DeleteSimple(subagentBundle.id)))
      .await(99.s)
    eventWatch.await[ItemDeleted](_.event.key == subagentBundle.id, after = eventId)

  "Delete Subagents" in:
    val eventId = eventWatch.lastAddedEventId
    controller.api
      .updateItems(Stream(
        DeleteSimple(bSubagentId),
        DeleteSimple(cSubagentId),
        DeleteSimple(dSubagentId)))
      .await(99.s).orThrow
    eventWatch.await[ItemDeleted](_.event.key == bSubagentId, after = eventId)
    eventWatch.await[ItemDeleted](_.event.key == cSubagentId, after = eventId)
    eventWatch.await[ItemDeleted](_.event.key == dSubagentId, after = eventId)


object SubagentBundleTest:
  private val localSubagentId = toLocalSubagentId(agentPath)
  private val aSubagentId = SubagentId("A-SUBAGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")
  private val cSubagentId = SubagentId("C-SUBAGENT")
  private val dSubagentId = SubagentId("D-SUBAGENT")

  private def newSubagentItem(id: SubagentId) =
    SubagentItem(id, agentPath, findFreeLocalUri())

  private val subagentBundle = SubagentBundle(
    SubagentBundleId("BUNDLE"),
    Map(
      aSubagentId -> NumericConstant(1),
      bSubagentId -> NumericConstant(2),
      cSubagentId -> NumericConstant(3),
      dSubagentId -> NumericConstant(3)))

  private val versionId = VersionId("VERSION")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ versionId,
    Seq(
      EmptyJob.execute(
        agentPath,
        subagentBundleId = Some(StringConstant(subagentBundle.id.string)),
        processLimit = 100)))

  private def toOrder(orderId: OrderId) =
    FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
