package js7.tests.subagent

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.item.VersionId
import js7.data.order.OrderEvent.OrderProcessingStarted
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentDisabledTest._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec

final class SubagentDisabledTest extends AnyFreeSpec with SubagentTester
{
  override protected def agentConfig = config"""
    js7.auth.subagents.A-SUBAGENT = "AGENT-PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "AGENT-PASSWORD"
    """.resolveWith(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected def items = Seq(workflow, aSubagentItem, bSubagentItem)

  private lazy val aSubagentItem = newSubagentItem(aSubagentId)
  private lazy val bSubagentItem = newSubagentItem(bSubagentId)

  protected implicit val scheduler = Scheduler.global

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
  }

  override def afterAll() = {
    Task.parZip2(aSubagentRelease, bSubagentRelease).await(99.s)
    super.afterAll()
  }

  "All Subagents enabled" in {
    runOrderAndCheck(localSubagentId)
    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)

    runOrderAndCheck(localSubagentId)
    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)
  }

  "Disable localSubagentId" in {
    controllerApi
      .updateItems(Observable(AddOrChangeSimple(
        localSubagentItem.copy(disabled = true))))
      .await(99.s).orThrow

    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)

    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(bSubagentId)
  }

  "Disable aSubagentId" in {
    controllerApi
      .updateItems(Observable(AddOrChangeSimple(
        aSubagentItem.copy(disabled = true))))
      .await(99.s).orThrow

    runOrderAndCheck(bSubagentId)
    runOrderAndCheck(bSubagentId)
  }

  "Enable aSubagentId, disable bSubagentId" in {
    controllerApi
      .updateItems(Observable(
        AddOrChangeSimple(
          aSubagentItem.copy(disabled = false)),
        AddOrChangeSimple(
          bSubagentItem.copy(disabled = true))))
      .await(99.s).orThrow

    runOrderAndCheck(aSubagentId)
    runOrderAndCheck(aSubagentId)
  }

  private def runOrderAndCheck(subagentId: SubagentId): Assertion =
    runOrderAndCheck(nextOrderId(), subagentId)

  private def runOrderAndCheck(orderId: OrderId, subagentId: SubagentId): Assertion = {
    val eventId = eventWatch.lastAddedEventId
    controllerApi.addOrder(toOrder(orderId)).await(99.s).orThrow
    val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
      .head.value.event
    assert(started.subagentId contains subagentId)
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
