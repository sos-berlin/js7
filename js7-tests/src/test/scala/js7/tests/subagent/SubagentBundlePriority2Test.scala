package js7.tests.subagent

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.traverse.*
import fs2.Stream
import js7.agent.motor.JobMotor
import js7.agent.{RunningAgent, TestAgent}
import js7.base.catsutils.Environment.TaggedResource
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isIntelliJIdea
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.order.FreshOrder
import js7.data.order.OrderEvent.OrderTerminated
import js7.data.subagent.SubagentItemStateEvent.SubagentCoupled
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.expression.Expression.convenience.given
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.Fork
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.subagent.jobs.TestJob
import js7.tests.subagent.SubagentBundlePriority2Test.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

final class SubagentBundlePriority2Test extends OurTestSuite, SubagentTester:

  private given ExecutionContext = ioRuntime.compute

  override protected def agentConfig = config"""
    js7.auth.subagents.A-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.C-SUBAGENT = "$localSubagentId's PASSWORD"
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  // TODO Probable only one Subagent is required, maybe the local one.
  private lazy val aSubagentItem = newSubagentItem(aSubagentId)
  private lazy val bSubagentItem = newSubagentItem(bSubagentId)
  private lazy val cSubagentItem = newSubagentItem(cSubagentId)
  private lazy val subagentItems = Seq(aSubagentItem, bSubagentItem, cSubagentItem)

  private var myAgent: TestAgent = null.asInstanceOf[TestAgent]
  private lazy val eventBus = StandardEventBus[JobMotor.TestOrderConcurrentlyChanged]()

  override protected def agentTestWiring = RunningAgent.TestWiring(
    envResources = Seq(TaggedResource(Resource.eval(IO:
      eventBus: EventPublisher[JobMotor.TestOrderConcurrentlyChanged]))))

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

  "Warning 'Order has been removed concurrently'" in:
    // TODO This test is expected to fail. See JobMotor for what to do.
    if !isIntelliJIdea then pending

    def myJob(sleep: FiniteDuration = 0.s) =
      TestJob.execute(agentPath, Map("sleep" -> sleep.toBigDecimalSeconds),
        subagentBundleId = Some("BUNDLE"), processLimit = 100)
    val workflow = Workflow(WorkflowPath("WORKFLOW"), Seq(
      myJob(),
      Fork.of(
        "1" -> Workflow.of(
          myJob(),
          myJob()),
        "2" -> Workflow.of(
          myJob(10.ms),
          myJob()),
        "3" -> Workflow.of(
          myJob(20.ms),
          myJob()),
        "4" -> Workflow.of(
          myJob(30.ms),
          myJob()),
        "5" -> Workflow.of(
          myJob(40.ms),
          myJob())),
      myJob()))

    val subagentBundle = SubagentBundle(
      SubagentBundleId("BUNDLE"),
      subagentToPriority = Map(
        aSubagentId ->
          // Number of processes started via our subagent bundle, allow 1
          expr"if $$js7ClusterSubagentProcessCount < 1 then 1 else missing",
        bSubagentId ->
          expr"if $$js7ClusterSubagentProcessCount < 1 then 1 else missing",
        cSubagentId ->
          expr"if $$js7ClusterSubagentProcessCount < 1 then 1 else missing"))

    var orderConcurrentlyChanged = false
    eventBus.when[JobMotor.TestOrderConcurrentlyChanged].map: evt =>
      Logger.error(s"$evt")
      orderConcurrentlyChanged = true

    withItems((subagentBundle, workflow)): _ =>
      (1 to 100).foreach: i =>
        logger.info("•" * 100 + i)
        val eventId = controller.resetLastWatchedEventId()
        val n = 1
        val orderIds = Vector.fill(n)(nextOrderId())
        controller.api.addOrders:
          Stream.iterable(orderIds)
            .map(FreshOrder(_, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow

        orderIds.foreach: orderId =>
          eventWatch.awaitNextKey[OrderTerminated](orderId, after = eventId, timeout = 999.s)
        assert(!orderConcurrentlyChanged)


object SubagentBundlePriority2Test:
  private val logger = Logger[this.type]
  private val localSubagentId = toLocalSubagentId(agentPath)
  private val aSubagentId = SubagentId("A-SUBAGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")
  private val cSubagentId = SubagentId("C-SUBAGENT")

  private def newSubagentItem(id: SubagentId) =
    SubagentItem(id, agentPath, findFreeLocalUri())
