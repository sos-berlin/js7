package js7.tests.subagent

import cats.effect.IO
import cats.syntax.traverse.*
import js7.base.catsutils.Environment.TaggedResource
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.VersionId
import js7.data.order.OrderEvent.OrderProcessingStarted
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentItemStateEvent.SubagentCoupled
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.web.GenericEventRoute
import js7.subagent.Subagent
import js7.tests.jobs.EmptyJob
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

  "Start and attach Subagents and SubagentBundle" in :
    // Start Subagents
    idToRelease

    updateItems(workflow :: subagentBundle :: subagentItems*): @unchecked

    for id <- subagentItems.map(_.id) do
      eventWatch.await[ItemAttached](_.event.key == id)
      eventWatch.await[SubagentCoupled](_.key == id)

    val orderId = nextOrderId()
    val events = controller.runOrder(FreshOrder(orderId, workflow.path))
      .map(_.value)

    // bSubagentId has the highest testMeteringValue
    assert:
      events.collectFirst:
        case OrderProcessingStarted(Some(subagentId), _) => subagentId
      == Some(bSubagentId)


object SubagentPriorityTest:

  private val localSubagentId = toLocalSubagentId(agentPath)
  private val aSubagentId = SubagentId("A-SUBAGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")

  private def newSubagentItem(id: SubagentId) =
    SubagentItem(id, agentPath, findFreeLocalUri())

  private val subagentBundle = SubagentBundle(
    SubagentBundleId("BUNDLE"),
    Map(
      aSubagentId -> expr("$testMeteringValue"),
      bSubagentId -> expr("$testMeteringValue")))

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW"),
    Seq(
      EmptyJob.execute(
        agentPath,
        subagentBundleId = Some(StringConstant(subagentBundle.id.string)),
        processLimit = 100)))

  private def toOrder(orderId: OrderId) =
    FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
