package js7.tests.subagent

import java.util.concurrent.TimeoutException
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAttached, OrderDeleted, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentDisabledTest._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class SubagentDisabledTest extends AnyFreeSpec with SubagentTester
{
  override protected def agentConfig = config"""
    js7.auth.subagents.A-SUBAGENT = "AGENT-PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "AGENT-PASSWORD"
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected def items = Seq(workflow, aSubagentItem, bSubagentItem)

  private lazy val aSubagentItem = newSubagentItem(aSubagentId)
  private lazy val bSubagentItem = newSubagentItem(bSubagentId)

  protected implicit val scheduler = Scheduler.traced

  private val localSubagentItem = directoryProvider.subagentItems.head
  private val localSubagentId = localSubagentItem.id
  private val nextOrderId = Iterator.from(1).map(i => OrderId(s"ORDER-$i")).next _

  private lazy val (aSubagent, aSubagentRelease) = subagentResource(aSubagentItem).allocated
    .await(99.s)
  private lazy val (bSubagent, bSubagentRelease) = subagentResource(bSubagentItem).allocated
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
    enableSubagents(localSubagentItem -> false)

    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)

    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)
  }

  "Disable aSubagentId" in {
    enableSubagents(aSubagentItem -> false)
    runOrderAndCheck(bSubagentId)
    runOrderAndCheck(bSubagentId)
  }

  "Enable aSubagentId, disable bSubagentId" in {
    enableSubagents(aSubagentItem -> true, bSubagentItem -> false)
    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(aSubagentId)
  }

  "Disable all Subagents including Director, then re-enableSubagents one" in {
    enableSubagents(aSubagentItem -> false)

    val orderId = nextOrderId()
    var eventId = eventWatch.lastAddedEventId
    controllerApi.addOrder(toOrder(orderId)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    eventId = eventWatch.lastAddedEventId
    intercept[TimeoutException] {
      eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId, timeout = 200.ms)
    }

    // Re-enableSubagents
    eventId = eventWatch.lastAddedEventId
    controllerApi
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
    controllerApi.addOrder(toOrder(orderId)).await(99.s).orThrow
    val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
      .head.value.event
    assert(started.subagentId contains subagentId)
    eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)
  }
}

object SubagentDisabledTest
{
  private val agentPath = AgentPath("AGENT")
  private val aSubagentId = SubagentId("A-SUBAGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")

  private def newSubagentItem(id: SubagentId) =
    SubagentItem(id, agentPath, Uri(s"http://localhost:${findFreeTcpPort()}"))

  private val versionId = VersionId("VERSION")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ versionId,
    Seq(
      EmptyJob.execute(agentPath, parallelism = 100)))

  private def toOrder(orderId: OrderId) =
    FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
}
