package js7.tests.internaljob

import cats.effect.IO
import cats.effect.std.Semaphore
import fs2.Stream
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.configutils.Configs.*
import js7.base.fs2utils.StreamExtensions.tapEach
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichPartialFunction}
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.event.{EventRequest, KeyedEvent}
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.VersionId
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderCancellationMarked, OrderFailed, OrderFinished, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.expression.Expression.{NamedValue, StringConstant}
import js7.data.value.{NamedValues, NumberValue, Value}
import js7.data.workflow.WorkflowPrinter.instructionToString
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath, WorkflowPrinter}
import js7.launcher.OrderProcess
import js7.launcher.forjava.internal.tests.{EmptyBlockingInternalJob, EmptyJInternalJob, TestBlockingInternalJob, TestJInternalJob}
import js7.launcher.internal.InternalJob
import js7.launcher.internal.InternalJob.JobContext
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.internaljob.InternalJobTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import org.scalactic.source
import org.scalatest.Assertions.*
import scala.collection.mutable
import scala.reflect.ClassTag

final class InternalJobTest
  extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  protected val agentPaths = agentPath :: Nil
  protected val items = Nil
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms
    """
  override protected val agentConfig = config"""
    js7.thread-pools.long-blocking.virtual = false
    js7.job.execution.signed-script-injection-allowed = on
    """
  private val versionIdIterator = Iterator.from(1).map(i => VersionId(s"v$i"))
  private val workflowPathIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i"))
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"🔷-$i"))
  private val testCounter = Atomic(0)

  "One InternalJob.start for multiple InternalJob.toOrderProcess" in:
    val versionId = versionIdIterator.next()
    val workflow = Workflow.of(workflowPathIterator.next(), execute_[AddOneJob])
    withTemporaryItem(workflow): workflow =>
      directoryProvider.updateVersionedItems(controller, versionId, Seq(workflow))

      for processNumber <- 1 to 3 do withClue(s"#$processNumber "):
        val order = FreshOrder(orderIdIterator.next(), workflow.path, Map("ORDER_ARG" -> NumberValue(300)))
        val events = controller.runOrder(order).map(_.value)

        val outcomes = events.collect { case OrderProcessed(outcome) => outcome }
        assert(outcomes == Vector(OrderOutcome.Succeeded(
          NamedValues(
            "START" -> NumberValue(1),  // One start only for multiple toOrderProcess calls
            "PROCESS" -> NumberValue(processNumber),
            "RESULT" -> NumberValue(301)))))

  addInternalJobTest(
    execute_[AddOneJob],
    orderArguments = Map("ORDER_ARG" -> NumberValue(100)),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues(
      "START" -> NumberValue(1),
      "PROCESS" -> NumberValue(1),
      "RESULT" -> NumberValue(101))))

  addInternalJobTest(
    execute_[AddOneJob],
    orderArguments = Map("ORDER_ARG" -> NumberValue(200)),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues(
      "START" -> NumberValue(1),
      "PROCESS" -> NumberValue(1),  // 1 again, because it is a different WorkflowJob
      "RESULT" -> NumberValue(201))))

  addInternalJobTest(
    Execute(WorkflowJob(agentPath, InternalExecutable(classOf[SimpleJob.type].getName))),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues.empty))

  "When workflow of last test has been deleted, the SimpleJob is stopped" in:
    awaitAndAssert(SimpleJob.stopped)

  addInternalJobTest(
    Execute(WorkflowJob(agentPath, InternalExecutable(classOf[EmptyJob].getName))),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues.empty))

  addInternalJobTest(
    Execute(WorkflowJob(agentPath, InternalExecutable(classOf[EmptyJInternalJob].getName))),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues.empty))

  addInternalJobTest(
    Execute(WorkflowJob(agentPath, InternalExecutable(classOf[EmptyBlockingInternalJob].getName))),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues.empty))

  for jobClass <- Seq(classOf[TestJInternalJob], classOf[TestBlockingInternalJob]) do
    jobClass.getName - {
      val n = 10
      lazy val indexedOrderIds = (1 to n).map(_ -> orderIdIterator.next())
      addInternalJobTestWithMultipleOrders(
        Execute(WorkflowJob(
          agentPath,
          InternalExecutable(
            jobClass.getName,
            script = "TEST SCRIPT",
            jobArguments = Map("blockingThreadNamePrefix" -> StringConstant(blockingThreadNamePrefix)),
            arguments = Map("STEP_ARG" -> NamedValue("ORDER_ARG"))),
          processLimit = n)),
        indexedOrderIds
          .map: (i, orderId) =>
            orderId -> Map("ORDER_ARG" -> NumberValue(i))
          .toMap,
        expectedOutcomes = indexedOrderIds
          .map: (i, orderId) =>
            orderId -> Seq(OrderOutcome.Succeeded(Map("RESULT" -> NumberValue(i + 1))))
          .toMap)
    }

  "Kill InternalJob" in:
    assert(CancelableJob.semaphore.flatMap(_.count).await(99.s) == 0)
    testCancelJob[CancelableJob]()
    assert(CancelableJob.semaphore.flatMap(_.count).await(99.s) == 0)

  "Kill JavaBlockingJob" in:
    testCancelJob[JCancelableJob]()

  private def testCancelJob[J: ClassTag](): Unit =
    val versionId = versionIdIterator.next()
    val workflow = Workflow.of(execute_[J]).withId(workflowPathIterator.next() ~ versionId)
    directoryProvider.updateVersionedItems(controller, versionId, Seq(workflow))

    val order = FreshOrder(orderIdIterator.next(), workflow.path, Map("ORDER_ARG" -> NumberValue(1)))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    sleep(300.ms) // Wait until OrderProcess has been started

    // Let the cancel handler throw its exception
    controller.api.executeCommand:
      CancelOrders(Seq(order.id), CancellationMode.kill())
    .await(99.s).orThrow
    eventWatch.await[OrderCancellationMarked](_.key == order.id)

    // Now cancel immediately
    controller.api.executeCommand:
      CancelOrders(Seq(order.id), CancellationMode.kill(immediately = true))
    .await(99.s).orThrow
    val outcome = eventWatch.await[OrderProcessed](_.key == order.id).head.value.event.outcome
    assert(outcome == OrderOutcome.Killed(OrderOutcome.Failed(Some("Canceled"))))
    eventWatch.await[OrderProcessingKilled](_.key == order.id)

  "stop" in:
    controller.api.stop await 99.s
    controller.terminate() await 99.s
    agent.terminate() await 99.s
    assert(SimpleJob.stopped)
    assert(TestJInternalJob.stoppedCalled.containsKey(blockingThreadNamePrefix))
    assert(TestBlockingInternalJob.stoppedCalled.containsKey(blockingThreadNamePrefix))

  private def addInternalJobTest(
    execute: Execute,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutcome: OrderOutcome)
    (using source.Position)
  : Unit =
    val orderId = orderIdIterator.next()
    addInternalJobTestWithMultipleOrders(execute,
      Map(orderId -> orderArguments),
      Map(orderId -> Seq(expectedOutcome)))

  private def addInternalJobTestWithMultipleOrders(
    execute: Execute,
    orderArguments: Map[OrderId, Map[String, Value]],
    expectedOutcomes: Map[OrderId, Seq[OrderOutcome]])
    (using source.Position)
  : Unit =
    val testName = testCounter.incrementAndGet().toString + ") " + instructionToString(execute)
    testName in:
      testWithWorkflow(Workflow.of(execute), orderArguments, expectedOutcomes)

  private def testWithWorkflow(
    anonymousWorkflow: Workflow,
    ordersArguments: Map[OrderId, Map[String, Value]],
    expectedOutcomes: Map[OrderId, Seq[OrderOutcome]])
    (using source.Position)
  : Unit =
    val orderToEvents = runMultipleOrdersWithWorkflow(anonymousWorkflow, ordersArguments)
    val outcomes = orderToEvents.view
      .mapValues(_.collect { case OrderProcessed(outcome) => outcome })
      .toMap
    assert(outcomes == expectedOutcomes)

    for (orderId, events) <- orderToEvents do
      if expectedOutcomes(orderId).last.isSucceeded then
        assert(events.exists(_.isInstanceOf[OrderFinished]))
      else
        assert(events.exists(_.isInstanceOf[OrderFailed]))

  private def runMultipleOrdersWithWorkflow(
    anonymousWorkflow: Workflow,
    ordersArguments: Map[OrderId, Map[String, Value]])
  : Map[OrderId, Seq[OrderEvent]] =
    testPrintAndParse(anonymousWorkflow)

    val eventId = eventWatch.lastAddedEventId
    val workflow = anonymousWorkflow.withId(workflowPathIterator.next())
    var workflowId: WorkflowId = null
    val result = withTemporaryItem(workflow): workflow =>
      workflowId = workflow.id
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrders:
        Stream.iterable(ordersArguments).map: (orderId, args) =>
          FreshOrder(orderId, workflow.path, arguments = args, deleteWhenTerminated = true)
      .await(99.s).orThrow: @unchecked
      val orderIds = ordersArguments.keySet
      val _runningOrderIds = orderIds.to(mutable.Set)
      eventWatch
        .stream(EventRequest.singleClass[OrderEvent](eventId, Some(99.s)))
        .filter(stamped => orderIds contains stamped.value.key)
        .map(_.value)
        .tapEach:
          case KeyedEvent(orderId: OrderId, _: OrderTerminated) =>
            _runningOrderIds -= orderId
          case _ =>
        .takeThrough(_ => _runningOrderIds.nonEmpty)
        .compile
        .toVector
        .await(99.s)
        .groupMap(_.key)(_.event)

    // When Workflow has been deleted, its Jobs are stopped
    eventWatch.await[ItemDeleted](_.event.key == workflowId, after = eventId)
    result

  private def testPrintAndParse(anonymousWorkflow: Workflow): Unit =
    val workflowNotation = WorkflowPrinter.print(anonymousWorkflow.withoutSource)
    //val reparsedWorkflow = WorkflowParser.parse(workflowNotation).map(_.withoutSource)
    logger.debug(workflowNotation)
    //TODO assert(reparsedWorkflow == Right(anonymousWorkflow.withoutSource))


object InternalJobTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")

  private def execute_[A: ClassTag] =
    Execute:
      WorkflowJob(
        agentPath,
        InternalExecutable(
          implicitClass[A].getName,
          script = "TEST SCRIPT",
          arguments = Map(
            "STEP_ARG" -> NamedValue("ORDER_ARG"))))

  private object SimpleJob extends InternalJob:
    private var started = false
    var stopped = false

    override def start = IO:
      started = true
      Right(())

    override def stop = IO:
      assert(started)
      stopped = true

    def toOrderProcess(step: Step) =
      assert(started)
      OrderProcess(IO(OrderOutcome.succeeded))

  private final class AddOneJob(jobContext: JobContext) extends InternalJob:
    assertThat(jobContext.implementationClass == getClass)
    assertThat(jobContext.executable.script == "TEST SCRIPT")

    private val startCount = Atomic(0)
    private val processCount = Atomic(0)

    override def start = IO:
      startCount += 1
      Right(())

    override def stop =
      IO.raiseError(new RuntimeException("AddOneJob.stop FAILS"))

    def toOrderProcess(step: Step) =
      OrderProcess:
        IO:
          processCount += 1
          OrderOutcome.Completed.fromChecked:
            step.arguments
              .checked("STEP_ARG")
              .flatMap(_.toNumberValue)
              .map: value =>
                OrderOutcome.Succeeded(NamedValues(
                  "START" -> NumberValue(startCount.get()),
                  "PROCESS" -> NumberValue(processCount.get()),
                  "RESULT" -> NumberValue(value.number + 1)))

  private class CancelableJob extends InternalJob:
    def toOrderProcess(step: Step) =
       new OrderProcess.FiberCancelable:
         def run =
           CancelableJob.semaphore
             .flatMap(_.acquire)
             .as(OrderOutcome.succeeded)

         override def cancel(immediately: Boolean) =
           if immediately then
             super.cancel(immediately)
           else
             IO.raiseError(new RuntimeException("JOB'S CANCEL OPERATION THROWS EXCEPTION"))

  private object CancelableJob:
    val semaphore = Semaphore[IO](0).unsafeMemoize
