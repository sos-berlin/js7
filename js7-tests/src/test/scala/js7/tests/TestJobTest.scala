package js7.tests

import js7.base.configutils.Configs.*
import js7.base.io.yaml.YamlExtensions.yaml
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderStderrWritten, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.value.Value
import js7.data.value.Value.convenience.given
import js7.data.workflow.Workflow
import js7.data.workflow.position.Position
import js7.tests.TestJobTest.*
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectoryProvider}
import org.scalatest.Assertions
import org.scalatest.compatible.Assertion
import scala.concurrent.duration.Deadline
import scala.language.implicitConversions

/** Test the TestJob. */
final class TestJobTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "stdout" in:
    testStdouterr("stdout", "TEST-STDOUT", OrderStdoutWritten("TEST-STDOUT"))
    testStdouterr("stdout", 94, OrderStdoutWritten:
      """!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~""")

  "stderr" in:
    testStdouterr("stderr", "TEST-STDERR", OrderStderrWritten("TEST-STDERR"))
    testStdouterr("stderr", 94, OrderStderrWritten:
      """!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~""")

  private def testStdouterr(argName: String, value: Value, orderStdWritten: OrderStdWritten)
  : Assertion =
    withItem[Workflow](yaml"""---
      instructions:
      - TYPE: Execute.Anonymous
        job:
          agentPath: ${agentPath.string}
          executable:
            TYPE: InternalExecutable
            className: js7.subagent.jobs.TestJob
            arguments:
              $argName: $value
    """): workflow =>
      val stampedEvents = controller.runOrder:
        FreshOrder(OrderId("ORDER"), workflow.path, deleteWhenTerminated = true)
      assert(stampedEvents.map(_.value) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        orderStdWritten,
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFinished(),
        OrderDeleted))

  "sleep" in:
    withItem[Workflow](yaml"""---
      instructions:
      - TYPE: Execute.Anonymous
        job:
          agentPath: ${agentPath.string}
          executable:
            TYPE: InternalExecutable
            className: js7.subagent.jobs.TestJob
            arguments:
              stdout: "'🐟 Hello!'"
              sleep: "0.1" # 100ms
    """): workflow =>
      val t = Deadline.now
      controller.runOrder:
        FreshOrder(OrderId("ORDER"), workflow.path, deleteWhenTerminated = true)
      assert(t.elapsed > 100.ms)

  "fail" in:
    withItem[Workflow](yaml"""---
      instructions:
      - TYPE: Execute.Anonymous
        job:
          agentPath: ${agentPath.string}
          executable:
            TYPE: InternalExecutable
            className: js7.subagent.jobs.TestJob
            arguments:
              fail: "'TEST FAILURE'"
    """): workflow =>
      val t = Deadline.now
      val events = controller.runOrder:
        FreshOrder(OrderId("ORDER"), workflow.path, deleteWhenTerminated = true)
      val outcomes =
        events.map(_.value).collect:
          case OrderProcessed(outcome) => outcome
      assert(outcomes == Vector(OrderOutcome.Failed(Some("TEST FAILURE"))))
      assert(events.map(_.value).exists(_.isInstanceOf[OrderFailed]))


private object TestJobTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = DirectoryProvider.toLocalSubagentId(agentPath)
