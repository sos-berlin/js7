package js7.tests.subagent

import cats.effect.IO
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
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.order.OrderEvent.{OrderDeleted, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentItemStateEvent.SubagentCoupled
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.expression.Expression.convenience.given
import js7.data.value.expression.Expression.{MissingConstant, NumericConstant, StringConstant, expr}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.subagent.SubagentBundlePriorityTest.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.language.implicitConversions

final class SubagentBundlePriorityTest extends OurTestSuite, SubagentTester:

  override protected def agentConfig = config"""
    js7.auth.subagents.A-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.C-SUBAGENT = "$localSubagentId's PASSWORD"
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  private lazy val aSubagentItem = newSubagentItem(aSubagentId)
  private lazy val bSubagentItem = newSubagentItem(bSubagentId)
  private lazy val cSubagentItem = newSubagentItem(cSubagentId)
  private lazy val subagentItems = Seq(aSubagentItem, bSubagentItem, cSubagentItem)

  private var myAgent: TestAgent = null.asInstanceOf[TestAgent]

  private lazy val idToRelease: Map[SubagentId, IO[Unit]] =
    subagentItems.traverse: subagentItem =>
      directoryProvider.bareSubagentResource(subagentItem)
        .allocated
        .map(subagentItem.id -> _._2.unsafeMemoize)
    .await(99.s)
    .toMap

  override def beforeAll() =
    super.beforeAll()
    myAgent = agent
    startAndAttachSubagentIds()

  override def afterAll() =
    try
      idToRelease.values.toVector.sequence.await(99.s)
      myAgent.terminate().await(99.s)
    finally
      super.afterAll()

  private def startAndAttachSubagentIds() =
    // Start Subagents
    idToRelease

    controller.api.updateItems:
      Stream.iterable:
        subagentItems
      .map(AddOrChangeSimple(_))
    .await(99.s).orThrow

    for id <- subagentItems.map(_.id) do
      eventWatch.await[ItemAttached](_.event.key == id)
      eventWatch.await[SubagentCoupled](_.key == id)

  "$js7ClusterProcessCount and $js7ClusterProcessCountOrders" in:
    val subagentBundle = SubagentBundle(
      SubagentBundleId("BUNDLE"),
      subagentToPriority = Map(
        aSubagentId ->
          // Number of processes started via our subagent bundle, allow 1
          expr"if $$js7ClusterSubagentProcessCount < 1 then 1 else missing",
        bSubagentId ->
          // Number of processes in the subagent started via our subagent bundle, allow 2
          expr"if $$js7ClusterProcessCount < 2 then 2 else missing",
        cSubagentId -> NumericConstant(0)))

    val otherSubagentBundle = SubagentBundle(
      SubagentBundleId("OTHER"),
      subagentToPriority = Map(aSubagentId -> 1))

    val workflow = Workflow(
      WorkflowPath("WORKFLOW"),
      Seq:
        ASemaphoreJob.execute(
          agentPath,
          subagentBundleId = Some(StringConstant(subagentBundle.id.string)),
          processLimit = 100))

    val otherWorkflow = Workflow(
      WorkflowPath("OTHER"),
      Seq:
        BSemaphoreJob.execute(
          agentPath,
          subagentBundleId = Some(StringConstant(otherSubagentBundle.id.string)),
          processLimit = 100))

    withItems((subagentBundle, otherSubagentBundle, workflow, otherWorkflow)): _ =>
      val otherOrderId = OrderId("OTHER")
      addOrder(FreshOrder(otherOrderId, otherWorkflow.path))
      controller.awaitNextKey[OrderProcessingStarted](otherOrderId)

      val n = 6
      val orderIds = Vector.fill(n)(nextOrderId())
      val startedEvents = orderIds.map: orderId =>
        addOrder(orderId, workflow.path)
        controller.awaitNextKey[OrderProcessingStarted](orderId).head.value

      val result = startedEvents.flatMap(_.subagentId).groupMapReduce(identity)(_ => 1)(_ + _)
      assert(result == Map(
        aSubagentId -> 1,
        bSubagentId -> 2,
        cSubagentId -> 3))

      val eventId = controller.lastAddedEventId
      ASemaphoreJob.continue(n)
      orderIds.foreach: orderId =>
        eventWatch.awaitNextKey[OrderDeleted](orderId, after = eventId)

      BSemaphoreJob.continue()
      eventWatch.awaitNextKey[OrderDeleted](otherOrderId)

  "Subagents whose priority is 'missing' are not selected" in:
    val subagentBundle = SubagentBundle(
      SubagentBundleId("BUNDLE-MISSING"),
      subagentToPriority = Map(
        aSubagentId -> MissingConstant,
        bSubagentId -> 1,
        cSubagentId -> MissingConstant))

    val workflow = Workflow(
      WorkflowPath("WORKFLOW-MISSING"),
      Seq:
        EmptyJob.execute(
          agentPath,
          subagentBundleId = Some(StringConstant(subagentBundle.id.string)),
          processLimit = 100))

    withItems((subagentBundle, workflow)): _ =>
      val orderId = OrderId("MISSING-PRIORITY")
      addOrder(orderId, workflow.path)
      val started = controller.awaitNextKey[OrderProcessingStarted](orderId).head.value
      assert(started == OrderProcessingStarted(Some(bSubagentId), Some(subagentBundle.id)))
      controller.awaitNextKey[OrderFinished](orderId)


object SubagentBundlePriorityTest:
  private val localSubagentId = toLocalSubagentId(agentPath)
  private val aSubagentId = SubagentId("A-SUBAGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")
  private val cSubagentId = SubagentId("C-SUBAGENT")

  private def newSubagentItem(id: SubagentId) =
    SubagentItem(id, agentPath, findFreeLocalUri())

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]

  final class BSemaphoreJob extends SemaphoreJob(BSemaphoreJob)
  object BSemaphoreJob extends SemaphoreJob.Companion[BSemaphoreJob]
