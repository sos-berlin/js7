package js7.tests

import js7.base.utils.Assertions.assertThat
import js7.common.configutils.Configs._
import js7.common.scalautil.Logger
import js7.data.agent.AgentId
import js7.data.item.VersionId
import js7.data.job.InternalExecutable
import js7.data.job.internal.InternalJob
import js7.data.order.OrderEvent.{OrderFailed, OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.{NamedValues, NumberValue, Value}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath, WorkflowPrinter}
import js7.tests.InternalJobTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec

final class InternalJobTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = agentId :: Nil
  protected val versionedItems = Nil
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  private val versionIdIterator = Iterator.from(1).map(i => VersionId(s"v$i"))
  private val workflowPathIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i"))
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"🔵-$i"))

  addInternalJobTest(
    Execute(internalWorkflowJob),
    orderArguments = Map("ARG" -> NumberValue(100)),
    expectedOutcome = Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(101))))

  addInternalJobTest(
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[SimpleJob].getName))),
    expectedOutcome = Outcome.Succeeded(NamedValues.empty))

  addInternalJobTest(
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[EmptyJob].getName))),
    expectedOutcome = Outcome.Succeeded(NamedValues.empty))

  private def addInternalJobTest(
    execute: Execute,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutcome: Outcome)
    (implicit pos: source.Position)
  : Unit =
    WorkflowPrinter.instructionToString(execute) in {
      testWithWorkflow(Workflow.of(execute), orderArguments, Seq(expectedOutcome))
    }

  private def testWithWorkflow(
    anonymousWorkflow: Workflow,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutcomes: Seq[Outcome])
  : Unit = {
    val events = runWithWorkflow(anonymousWorkflow, orderArguments)
    val outcomes = events.collect { case OrderProcessed(outcome) => outcome }
    assert(outcomes == expectedOutcomes)

    if (expectedOutcomes.last.isSucceeded) assert(events.last.isInstanceOf[OrderFinished])
    else assert(events.last.isInstanceOf[OrderFailed])
  }

  private def runWithWorkflow(
    anonymousWorkflow: Workflow,
    orderArguments: Map[String, Value] = Map.empty)
  : Seq[OrderEvent] = {
    testPrintAndParse(anonymousWorkflow)

    val versionId = versionIdIterator.next()
    val workflow = anonymousWorkflow.withId(workflowPathIterator.next() ~ versionId)
    val order = FreshOrder(orderIdIterator.next(), workflow.path, arguments = orderArguments)
    directoryProvider.updateVersionedItems(controller, versionId, Seq(workflow))

    controller.runOrder(order).map(_.value)
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

  private final class SimpleJob // No constructor parameters
  extends InternalJob
  {
    def processOrder(context: InternalJob.OrderContext) =
      Task.pure(Right(NamedValues.empty))
  }

  private final class AddOneJob(jobContext: InternalJob.JobContext)
  extends InternalJob
  {
    assertThat(jobContext.workflowJob == internalWorkflowJob)

    def processOrder(context: InternalJob.OrderContext) =
      Task {
        for (number <- context.evalToBigDecimal("$ARG")) yield
          NamedValues("RESULT" -> NumberValue(number + 1))
      }
  }
}
