package js7.tests.subagent

import cats.effect.IO
import cats.syntax.traverse.*
import fs2.Stream
import js7.agent.TestAgent
import js7.base.catsutils.CatsEffectExtensions.orThrow
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.event.{EventRequest, KeyedEvent}
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderDeleted, OrderFinished, OrderProcessed, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.subagent.SubagentItemStateEvent.SubagentCoupled
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.expression.Expression.convenience.given
import js7.data.value.expression.Expression.{MissingConstant, NumericConstant, StringConstant, expr, exprFun}
import js7.data.value.expression.ExpressionParser.parseExpr
import js7.data.value.expression.{Expression, ExpressionParser}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ForkList}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.subagent.SubagentBundlePriorityTest.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import org.scalatest.Assertion
import scala.collection.mutable
import scala.language.implicitConversions

final class SubagentBundlePriorityTest extends OurTestSuite, SubagentTester:

  override protected def agentConfig = config"""
    js7.auth.subagents.A-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.C-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.subagent-driver.reconnect-delays = [10ms] // TODO Change SubagentKeeper polling wait routine
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
          // Number of processes in this Subagent started via our SubagentBundle, allow 1
          expr"if $$js7ClusterSubagentProcessCount < 1 then 1 else missing",
        bSubagentId ->
          // Number of processes started via our SubagentBundle, allow 3
          expr"if $$js7ClusterProcessCount < 3 then 2 else missing",
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

  "Massive test with Fork and SubagentBundle containing three Subagents" in:
    val orderCount = 100
    val perSubagentLimit = 2
    val bundleLimit = 5

    val subagentBundle = SubagentBundle(
      SubagentBundleId("MASSIVE-FORK"),
      subagentToPriority =
        def priorityExpr(priority: Int) = parseExpr:
          s"""if $$js7ClusterSubagentProcessCount < $perSubagentLimit
             | && $$js7ClusterProcessCount < $bundleLimit
             |then $priority
             |else missing
             |""".stripMargin
        Map(
          aSubagentId -> priorityExpr(3),
          bSubagentId -> priorityExpr(2),
          cSubagentId -> priorityExpr(1)))

    val workflow = Workflow(
      WorkflowPath("MASSIVE-FORK"),
      Seq:
        ForkList(
          children = expr"[1, 2, 3, 4]",
          childToId = exprFun"x => $$x",
          childToArguments = exprFun"x => { x: $$x }",
          Workflow.of(
            Execute(WorkflowJob(
              agentPath,
              ShellScriptExecutable(":"),
              subagentBundleId = Some(StringConstant(subagentBundle.id.string)),
              processLimit = 100)),
            Execute(WorkflowJob(
              agentPath,
              ShellScriptExecutable(":"),
              subagentBundleId = Some(StringConstant(subagentBundle.id.string)),
              processLimit = 100)))))

    withItems((subagentBundle, workflow)): _ =>
      var processCountMax = 0
      final case class Entry(var count: Int, var maximum: Int):
        def increment() =
          count += 1
          if maximum < count then maximum = count
      val subagentToProcessCount = subagentBundle.subagentIds.map(_ -> Entry(0, 0)).toMap
      val orderIds = (1 to orderCount).map(i => OrderId(s"MASSIVE-FORK-$i"))
      controller.api.addOrders:
        orderIds.map: orderId =>
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
      .orThrow
      .both:
        Stream.suspend:
          val unfinished = orderIds.to(mutable.Set)
          var processCount = 0
          val orderToSubagentId = mutable.Map[OrderId, SubagentId]()
          controller.eventWatch.stream[OrderEvent]:
            EventRequest.singleClass(after = controller.lastAddedEventId, timeout = None)
          .map(_.value)
          .map:
            case KeyedEvent(orderId, event) =>
              event match
                case event: OrderProcessingStarted =>
                  subagentToProcessCount(event.subagentId.get).increment()

                  processCount += 1
                  processCountMax = processCountMax max processCount
                  assert(processCount <= bundleLimit)

                  Logger.info(s"$orderId processCount=$processCount max=$processCountMax")
                  orderToSubagentId.put(orderId, event.subagentId.get)
                case _: OrderProcessed =>
                  processCount -= 1
                  assert(processCount >= 0)

                  val subagentId = orderToSubagentId.remove(orderId).get
                  subagentToProcessCount(subagentId).count -= 1
                  assert(subagentToProcessCount(subagentId).count >= 0)
                case _: OrderFinished =>
                  unfinished -= orderId
                case _ =>
          .takeWhile(_ => unfinished.nonEmpty)
        .compile.drain
      .await(99.s)
      assert(processCountMax == bundleLimit)
      assert(subagentToProcessCount(aSubagentId).maximum == perSubagentLimit)
      assert(subagentToProcessCount(bSubagentId).maximum == perSubagentLimit)
      assert(subagentToProcessCount(cSubagentId).maximum == bundleLimit - 2 * perSubagentLimit)


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
