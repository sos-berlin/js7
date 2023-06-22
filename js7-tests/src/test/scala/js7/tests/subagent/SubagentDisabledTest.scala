package js7.tests.subagent

import java.util.concurrent.TimeoutException
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAttached, OrderDeleted, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentDisabledTest.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

final class SubagentDisabledTest extends OurTestSuite with SubagentTester
{
  override protected def agentConfig = config"""
    js7.auth.subagents.A-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "$localSubagentId's PASSWORD"
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected def items = Seq(workflow, aSubagentItem, bSubagentItem)

  private lazy val aSubagentItem = newSubagentItem(aSubagentId)
  private lazy val bSubagentItem = newSubagentItem(bSubagentId)

  protected implicit val scheduler = Scheduler.traced

  private val nextOrderId = Iterator.from(1).map(i => OrderId(s"ORDER-$i")).next _

  private lazy val (aSubagent, aSubagentRelease) =
    subagentResource(aSubagentItem, director = toLocalSubagentId(agentPath))
      .allocated
      .await(99.s)
  private lazy val (bSubagent, bSubagentRelease) =
    subagentResource(bSubagentItem, director = toLocalSubagentId(agentPath))
      .allocated
      .await(99.s)

  override def beforeAll() = {
    super.beforeAll()
    aSubagent
    bSubagent
    eventWatch.await[ItemAttached](_.event.key == aSubagentId)
    eventWatch.await[ItemAttached](_.event.key == bSubagentId)
  }

  override def afterAll() = {
    Task.parZip2(aSubagentRelease, bSubagentRelease).await(99.s)
    super.afterAll()
  }

  "All Subagents are enabled" in {
    runOrderAndCheck(localSubagentId)
    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)

    runOrderAndCheck(localSubagentId)
    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)
  }

  "Disable localSubagentId" in {
    enableSubagents(localSubagentId -> false)

    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)

    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)
  }

  "Disable aSubagentId" in {
    enableSubagents(aSubagentItem.id -> false)
    runOrderAndCheck(bSubagentId)
    runOrderAndCheck(bSubagentId)
  }

  "Enable aSubagentId, disable bSubagentId" in {
    enableSubagents(aSubagentItem.id -> true, bSubagentItem.id -> false)
    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(aSubagentId)
  }

  "Disable all Subagents including Director, then re-enableSubagents one" in {
    enableSubagents(aSubagentItem.id -> false)

    val orderId = nextOrderId()
    var eventId = eventWatch.lastAddedEventId
    controller.api.addOrder(toOrder(orderId)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    eventId = eventWatch.lastAddedEventId
    intercept[TimeoutException] {
      eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId, timeout = 200.ms)
    }

    // Re-enableSubagents
    eventId = eventWatch.lastAddedEventId
    controller.api
      .updateItems(Observable(
        AddOrChangeSimple(aSubagentItem.copy(disabled = false))))
      .await(99.s).orThrow

    // Now, the enabled Subagent is selected and Order processing starts
    val started = eventWatch
      .await[OrderProcessingStarted](_.key == orderId, after = eventId)
      .head.value.event
    assert(started.subagentId.contains(aSubagentId))
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
  }

  private def runOrderAndCheck(subagentId: SubagentId): Unit =
    runOrderAndCheck(nextOrderId(), subagentId)

  private def runOrderAndCheck(orderId: OrderId, subagentId: SubagentId): Unit = {
    val eventId = eventWatch.lastAddedEventId
    controller.api.addOrder(toOrder(orderId)).await(99.s).orThrow
    val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
      .head.value.event
    assert(started.subagentId contains subagentId)
    eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)
  }
}

object SubagentDisabledTest
{
  private val localSubagentId = toLocalSubagentId(agentPath)
  private val aSubagentId = SubagentId("A-SUBAGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")

  private def newSubagentItem(id: SubagentId) =
    SubagentItem(id, agentPath, findFreeLocalUri())

  private val versionId = VersionId("VERSION")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ versionId,
    Seq(
      EmptyJob.execute(agentPath, parallelism = 100)))

  private def toOrder(orderId: OrderId) =
    FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
}
