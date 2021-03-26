package js7.tests

import js7.base.configutils.Configs._
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentId
import js7.data.event.{EventRequest, KeyedEvent}
import js7.data.item.VersionId
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderFailed, OrderFinished, OrderProcessed, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.Expression.{NamedValue, ObjectExpression}
import js7.data.value.{NamedValues, NumberValue, StringValue, Value}
import js7.data.workflow.WorkflowPrinter.instructionToString
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath, WorkflowPrinter}
import js7.executor.forjava.internal.tests.{EmptyBlockingInternalJob, EmptyJInternalJob, TestBlockingInternalJob, TestJInternalJob}
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.{JobContext, OrderContext, OrderProcess, Result}
import js7.tests.InternalJobTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

final class InternalJobTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = agentId :: Nil
  protected val versionedItems = Nil
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private val versionIdIterator = Iterator.from(1).map(i => VersionId(s"v$i"))
  private val workflowPathIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i"))
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"🔵-$i"))
  private val testCounter = AtomicInt(0)

  "One InternalJob.start for multiple InternalJob.processOrder" in {
    val versionId = versionIdIterator.next()
    val workflow = Workflow.of(Execute(internalWorkflowJob))
      .withId(workflowPathIterator.next() ~ versionId)
    directoryProvider.updateVersionedItems(controller, versionId, Seq(workflow))

    for (processNumber <- 1 to 3) withClue(s"#$processNumber ") {
      val order = FreshOrder(orderIdIterator.next(), workflow.path, Map("ARG" -> NumberValue(300)))
      val events = controller.runOrder(order).map(_.value)

      val outcomes = events.collect { case OrderProcessed(outcome) => outcome }
      assert(outcomes == Vector(Outcome.Succeeded(
        NamedValues(
          "START" -> NumberValue(1),  // One start only for muliple processOrder calls
          "PROCESS" -> NumberValue(processNumber),
          "RESULT" -> NumberValue(301)))))
    }
  }

  addInternalJobTest(
    Execute(internalWorkflowJob),
    orderArguments = Map("ARG" -> NumberValue(100)),
    expectedOutcome = Outcome.Succeeded(NamedValues(
      "START" -> NumberValue(1),
      "PROCESS" -> NumberValue(1),
      "RESULT" -> NumberValue(101))))

  addInternalJobTest(
    Execute(internalWorkflowJob),
    orderArguments = Map("ARG" -> NumberValue(200)),
    expectedOutcome = Outcome.Succeeded(NamedValues(
      "START" -> NumberValue(1),
      "PROCESS" -> NumberValue(1),  // 1 agein, because it is a different WorkflowJob
      "RESULT" -> NumberValue(201))))

  addInternalJobTest(
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[SimpleJob.type].getName))),
    expectedOutcome = Outcome.Succeeded(NamedValues.empty))

  addInternalJobTest(
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[EmptyJob].getName))),
    expectedOutcome = Outcome.Succeeded(NamedValues.empty))

  addInternalJobTest(
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[EmptyJInternalJob].getName))),
    expectedOutcome = Outcome.Succeeded(NamedValues.empty))

  addInternalJobTest(
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[EmptyBlockingInternalJob].getName))),
    expectedOutcome = Outcome.Succeeded(NamedValues.empty))

  for (jobClass <- Seq(classOf[TestJInternalJob], classOf[TestBlockingInternalJob]))
    jobClass.getName - {
      val n = 10
      lazy val indexedOrderIds = (1 to n).map(_ -> orderIdIterator.next())
      addInternalJobTestWithMultipleOrders(
        Execute(WorkflowJob(
          agentId,
          InternalExecutable(
            jobClass.getName,
            Map("expectedThreadPrefix" -> StringValue("JS7 blocking job ")),
            ObjectExpression(Map("arg" -> NamedValue.last("ARG")))),
          taskLimit = n)),
        indexedOrderIds
          .map { case (i, orderId) => orderId -> Map("ARG" -> NumberValue(i)) }
          .toMap,
        expectedOutcomes = indexedOrderIds
          .map { case (i, orderId) =>
            orderId -> Seq(Outcome.Succeeded(Map("RESULT" -> NumberValue(i + 1))))
          }
          .toMap)
    }

  private def addInternalJobTest(
    execute: Execute,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutcome: Outcome)
    (implicit pos: source.Position)
  : Unit = {
    val orderId = orderIdIterator.next()
    addInternalJobTestWithMultipleOrders(execute,
      Map(orderId -> orderArguments),
      Map(orderId -> Seq(expectedOutcome)))
  }
  private def addInternalJobTestWithMultipleOrders(
    execute: Execute,
    orderArguments: Map[OrderId, Map[String, Value]],
    expectedOutcomes: Map[OrderId, Seq[Outcome]])
    (implicit pos: source.Position)
  : Unit = {
    val testName = testCounter.incrementAndGet().toString + ") " + instructionToString(execute)
    testName in {
      testWithWorkflow(Workflow.of(execute), orderArguments, expectedOutcomes)
    }
  }

  private def testWithWorkflow(
    anonymousWorkflow: Workflow,
    ordersArguments: Map[OrderId, Map[String, Value]],
    expectedOutcomes: Map[OrderId, Seq[Outcome]])
  : Unit = {
    val orderToEvents = runMultipleOrdersWithWorkflow(anonymousWorkflow, ordersArguments)
    val outcomes = orderToEvents.view
      .mapValues(_.collect { case OrderProcessed(outcome) => outcome })
      .toMap
    assert(outcomes == expectedOutcomes)

    for ((orderId, events) <- orderToEvents) {
      if (expectedOutcomes(orderId).last.isSucceeded)
        assert(events.last.isInstanceOf[OrderFinished])
      else
        assert(events.last.isInstanceOf[OrderFailed])
    }
  }

  private def runMultipleOrdersWithWorkflow(
    anonymousWorkflow: Workflow,
    ordersArguments: Map[OrderId, Map[String, Value]])
  : Map[OrderId, Seq[OrderEvent]] = {
    testPrintAndParse(anonymousWorkflow)

    val versionId = versionIdIterator.next()
    val workflow = anonymousWorkflow.withId(workflowPathIterator.next() ~ versionId)
    directoryProvider.updateVersionedItems(controller, versionId, Seq(workflow))

    val eventId = controller.eventWatch.lastAddedEventId
    controllerApi.addOrders(
      Observable.fromIterable(ordersArguments)
        .map {
          case (orderId, args) => FreshOrder(orderId, workflow.path, arguments = args)
        })
      .await(99.s).orThrow
    val orderIds = ordersArguments.keySet
    val _runningOrderIds = orderIds.to(mutable.Set)
    controller.eventWatch
      .observe(EventRequest.singleClass[OrderEvent](eventId, Some(99.s)))
      .filter(stamped => orderIds contains stamped.value.key)
      .map(_.value)
      .tapEach {
        case KeyedEvent(orderId: OrderId, _: OrderTerminated) =>
          _runningOrderIds -= orderId
        case _ =>
      }
      .takeWhileInclusive(_ => _runningOrderIds.nonEmpty)
      .toL(Vector)
      .await(99.s)
      .groupMap(_.key)(_.event)
  }

  private def testPrintAndParse(anonymousWorkflow: Workflow): Unit = {
    val workflowNotation = WorkflowPrinter.print(anonymousWorkflow.withoutSource)
    val reparsedWorkflow = WorkflowParser.parse(workflowNotation).map(_.withoutSource)
    logger.debug(workflowNotation)
    assert(reparsedWorkflow == Right(anonymousWorkflow.withoutSource))
  }
}

object InternalJobTest
{
  private val logger = Logger(getClass)
  private val agentId = AgentId("AGENT")
  private val internalWorkflowJob = WorkflowJob(
    agentId,
    InternalExecutable(classOf[AddOneJob].getName))

  private object SimpleJob extends InternalJob
  {
    def processOrder(context: OrderContext) =
      OrderProcess(
        Task {
          Right(Result(NamedValues.empty))
        })
  }

  private final class AddOneJob(jobContext: JobContext) extends InternalJob
  {
    assertThat(jobContext.implementationClass == getClass)
    private val startCount = AtomicInt(0)
    private val processCount = AtomicInt(0)

    override def start = Task {
      startCount += 1
      Right(())
    }

    def processOrder(context: OrderContext) =
      OrderProcess(
        Task {
          processCount += 1
          for (number <- context.scope.evalToBigDecimal("$ARG")) yield
            Result(NamedValues(
              "START" -> NumberValue(startCount.get()),
              "PROCESS" -> NumberValue(processCount.get()),
              "RESULT" -> NumberValue(number + 1)))
        })
  }
}
