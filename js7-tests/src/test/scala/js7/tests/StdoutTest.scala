package js7.tests

import js7.base.configutils.Configs._
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isWindows
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AgentId
import js7.data.item.VersionId
import js7.data.job.{Executable, ScriptExecutable}
import js7.data.order.OrderEvent.OrderStdoutWritten
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.value.{NamedValues, NumberValue, Value}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath, WorkflowPrinter}
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.{OrderContext, OrderProcess, Result}
import js7.tests.StdoutTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

final class StdoutTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(agentId)
  protected val versionedItems = Nil
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.order.stdout-stderr.chunk-size = $chunkSize
    js7.order.stdout-stderr.delay = ${delay.toMillis}ms"""

  private val versionIdIterator = Iterator.from(1).map(i => VersionId(s"v$i"))
  private val workflowPathIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i"))
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"🔵-$i"))

  "stdout event compacting" in {
    def runTest() =
      testExecutable(
        ScriptExecutable(
          s"""echo A
            |${sleepCommand(longDelay)}
            |echo B-1
            |echo B-2
            |${sleepCommand(shortDelay)}
            |echo B-3
            |echo B-4
            |${sleepCommand(longDelay)}
            |echo C
            |${sleepCommand(longDelay)}
            |echo D-1
            |echo D-2
            |${sleepCommand(shortDelay)}
            |echo D-3
            |${sleepCommand(longDelay)}
            |echo .........1........2
            |echo .........3........4
            |${sleepCommand(shortDelay)}
            |echo .........5........6
            |echo .........7........8
            |echo .........9.......10
            |""".stripMargin),
        Seq(
          "A\n",
          "B-1\n" + "B-2\n" + "B-3\n" + "B-4\n",
          "C\n",
          "D-1\n" + "D-2\n" + "D-3\n",
          ".........1........2\n.........3........4\n",
          ".........5........6\n.........7........8\n.........9",
          ".......10\n"))
    Try(runTest())  // Warm-up
    runTest()
  }

  private def testExecutable(
    executable: Executable,
    expectedChunks: Seq[String])
    (implicit pos: source.Position)
  : Unit =
    testWithWorkflow(
      Workflow.of(Execute(WorkflowJob(agentId, executable))),
      expectedChunks)

  private def testWithWorkflow(
    anonymousWorkflow: Workflow,
    expectedChunks: Seq[String])
  : Unit = {
    val events = runWithWorkflow(anonymousWorkflow)
    val chunks = events.collect { case OrderStdoutWritten(chunk) => chunk }
    assert(chunks == expectedChunks)
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

object StdoutTest
{
  private val logger = Logger(getClass)
  private val agentId = AgentId("AGENT")
  private val chunkSize = 50
  private val delay = 1000.ms
  private val shortDelay = 100.ms
  private val longDelay = delay + 500.ms

  private def sleepCommand(delay: FiniteDuration) =
    if (isWindows) s"SLEEP ${delay.toDecimalString}\n"  // TODO Windows
    else s"sleep ${delay.toDecimalString}\n"

  private final class TestInternalJob extends InternalJob
  {
    def processOrder(context: OrderContext) =
      OrderProcess(
        Task {
          for (number <- context.arguments.checked("ARG").flatMap(_.toNumber).map(_.number)) yield
            Result(NamedValues("RESULT" -> NumberValue(number + 1)))
        })
  }
}
