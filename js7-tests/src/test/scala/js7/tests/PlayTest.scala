package js7.tests

import io.circe.Json
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.test.Test
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AgentPath
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderFailed, OrderFinished, OrderProcessed, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.{NamedValues, NumberValue, Value}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.testenv.ControllerAgentForScalaTest

/* A test to play with. */
final class PlayTest extends Test with ControllerAgentForScalaTest
{
  protected val agentPaths = PlayTest.agentPaths
  protected val items = Nil
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private val versionIdIterator = Iterator.from(1).map(i => VersionId(s"v$i"))
  private val workflowPathIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i"))
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"🔵-$i"))

  "Test" in {
    val workflowJson = json"""{
      "instructions": []
    }"""

    testWithWorkflow(toWorkflow(workflowJson),
      Map.empty,
      expectedOutput = "",
      expectedOutcomes = Nil)
  }

  private def toWorkflow(json: Json): Workflow =
    json.as(Workflow.topJsonDecoder).toChecked.orThrow

  private def testWithWorkflow(
    anonymousWorkflow: Workflow,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutput: String,
    expectedOutcomes: Seq[Outcome])
  : Unit = {
    val events = runWithWorkflow(anonymousWorkflow, orderArguments)
    val output = events.collect { case OrderStdoutWritten(chunk) => chunk }.fold("")(_ + _)
    val outcomes = events.collect { case OrderProcessed(outcome) => outcome }
    assert(output == expectedOutput && outcomes == expectedOutcomes)
    if (expectedOutcomes.lastOption.forall(_.isSucceeded))
      assert(events.last.isInstanceOf[OrderFinished])
    else
      assert(events.last.isInstanceOf[OrderFailed])
  }

  private def runWithWorkflow(
    anonymousWorkflow: Workflow,
    orderArguments: Map[String, Value] = Map.empty)
  : Seq[OrderEvent] = {
    val versionId = versionIdIterator.next()
    val workflow = anonymousWorkflow.withId(workflowPathIterator.next() ~ versionId)
    val order = FreshOrder(orderIdIterator.next(), workflow.path, arguments = orderArguments)
    directoryProvider.updateVersionedItems(controller, versionId, Seq(workflow))

    controller.runOrder(order).map(_.value)
  }
}

object PlayTest
{
  private val logger = Logger(getClass)
  private val agentPaths = Seq(AgentPath("primaryAgent"), AgentPath("secondaryAgent"))  // Names for Andreas' test cases

  private final class TestInternalJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess.fromCheckedOutcome(
        for (number <- step.arguments.checked("ARG").flatMap(_.toNumberValue).map(_.number)) yield
          Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(number + 1))))
  }
}
