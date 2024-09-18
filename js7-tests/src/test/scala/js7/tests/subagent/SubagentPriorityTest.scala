package js7.tests.subagent

import cats.effect.IO
import cats.syntax.traverse.*
import js7.base.catsutils.Environment.TaggedResource
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.order.OrderEvent.{OrderProcessingStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentItemStateEvent.SubagentCoupled
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.web.GenericEventRoute
import js7.subagent.Subagent
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.subagent.SubagentPriorityTest.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.BlockingItemUpdater
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class SubagentPriorityTest extends OurTestSuite, SubagentTester, BlockingItemUpdater:

  override protected def agentConfig = config"""
    js7.auth.subagents.A-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "$localSubagentId's PASSWORD"
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  private lazy val aSubagentItem = newSubagentItem(aSubagentId)
  private lazy val bSubagentItem = newSubagentItem(bSubagentId)
  private lazy val subagentItems = List(aSubagentItem, bSubagentItem)

  private val nextOrderId = Iterator.from(1).map(i => OrderId(s"ORDER-$i")).next _

  private lazy val idToRelease = subagentItems.zipWithIndex
    .traverse: (subagentItem, i) =>
      directoryProvider.bareSubagentResource(
          subagentItem,
          testWiring = Subagent.TestWiring(
            envResources = Seq:
              TaggedResource.eval(IO:
                GenericEventRoute.TestWiring(testMeteringValue = Some(i)))))
        .allocated
        .map(subagentItem.id -> _._2.unsafeMemoize)
    .await(99.s)
    .toMap

  override def afterAll() =
    try
      idToRelease.values.toVector.sequence.await(99.s)
    finally
      super.afterAll()

  private lazy val startSubagets: Unit =
    idToRelease

  "Use $js7TestMeteringValue from Subagent" in :
    startSubagets

    val subagentBundle = SubagentBundle(
      SubagentBundleId("BUNDLE-1"),
      Map(
        aSubagentId -> expr("$js7TestMeteringValue"),
        bSubagentId -> expr("$js7TestMeteringValue")))

    val workflow = Workflow(
      WorkflowPath("WORKFLOW-1"),
      Seq(
        EmptyJob.execute(
          agentPath,
          subagentBundleId = Some(StringConstant(subagentBundle.id.string)),
          processLimit = 100)))

    updateItems(workflow :: subagentBundle :: subagentItems*)
    for id <- subagentItems.map(_.id) do
      eventWatch.await[ItemAttached](_.event.key == id)
      eventWatch.await[SubagentCoupled](_.key == id)

    val orderId = nextOrderId()
    val events = controller
      .runOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .map(_.value)

    // bSubagentId has the highest testMeteringValue
    assert:
      events.collectFirst:
        case OrderProcessingStarted(Some(subagentId), Some(_), false) => subagentId
      == Some(bSubagentId)

    eventWatch.await[OrderTerminated](_.key == orderId)
    deleteItems(workflow.path, subagentBundle.path)

  "Use $js7SubagentProcessCount and $js7BundleSubagentProcessCount" in :
    startSubagets

    val subagentBundle = SubagentBundle(
      SubagentBundleId("BUNDLE-2"),
      Map(
        aSubagentId -> expr("1"),
        bSubagentId -> expr("2 - ($js7SubagentProcessCount + $js7BundleSubagentProcessCount)")))

    val workflow = Workflow(
      WorkflowPath("WORKFLOW-2"),
      Seq(
        ASemaphoreJob.execute(
          agentPath,
          subagentBundleId = Some(StringConstant(subagentBundle.id.string)),
          processLimit = 100)))

      updateItems(workflow :: subagentBundle :: subagentItems*)
      for id <- subagentItems.map(_.id) do
        eventWatch.await[ItemAttached](_.event.key == id)
        eventWatch.await[SubagentCoupled](_.key == id)

      val aOrderId, bOrderId = nextOrderId()

      controller.api.addOrder(FreshOrder(aOrderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      val aStarted = eventWatch.awaitNext[OrderProcessingStarted](_.key == aOrderId)
        .head.value.event

      controller.api.addOrder(FreshOrder(bOrderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      val bStarted = eventWatch.awaitNext[OrderProcessingStarted](_.key == bOrderId)
        .head.value.event

      assert(aStarted.subagentId == Some(bSubagentId) && bStarted.subagentId == Some(aSubagentId))

      ASemaphoreJob.continue(2)
      eventWatch.await[OrderTerminated](_.key == aOrderId)
      eventWatch.await[OrderTerminated](_.key == bOrderId)

      deleteItems(workflow.path, subagentBundle.path)


object SubagentPriorityTest:

  private val localSubagentId = toLocalSubagentId(agentPath)
  private val aSubagentId = SubagentId("A-SUBAGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")

  private def newSubagentItem(id: SubagentId) =
    SubagentItem(id, agentPath, findFreeLocalUri())

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
