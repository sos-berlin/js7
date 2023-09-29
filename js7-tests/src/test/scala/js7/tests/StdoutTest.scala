package js7.tests

import js7.base.configutils.Configs.*
import js7.base.io.process.Stdout
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.item.VersionId
import js7.data.job.{Executable, ShellScriptExecutable}
import js7.data.order.OrderEvent.OrderStdoutWritten
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.Value
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath, WorkflowPrinter}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.StdoutTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

final class StdoutTest extends OurTestSuite with ControllerAgentForScalaTest:
  protected val agentPaths = Seq(agentPath)
  protected val items = Nil
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.order.stdout-stderr.chunk-size = $chunkSize
    js7.order.stdout-stderr.delay = $delay"""

  private val versionIdIterator = Iterator.from(1).map(i => VersionId(s"v$i"))
  private val workflowPathIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i"))
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"🔷-$i"))

  "ShellScriptExecutable OrderStdoutWritten event compacting" in:
    // Shell stdout timing seems not to be reliable !!!
    // The other test with InternalExecutable should be adequate.
    pending

    def runTest() =
      testExecutable(
        ShellScriptExecutable(
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
            |${sleepCommand(longDelay)}
            |echo .........5........6
            |echo .........7........8
            |echo .........9.......10
            |echo ........11........12........13........14........15.......16
            |""".stripMargin),
        Seq(
          "A\n",
          "B-1\n" + "B-2\n" + "B-3\n" + "B-4\n",
          "C\n",
          "D-1\n" + "D-2\n" + "D-3\n",
          ".........1........2\n.........3........4\n",
          // stdout InputStreamReader is expected to return the strings already properly chunked
          // - ".........5........6\n.........7........8\n.........9"
          // - ".......10\n........11........12........13........14"
          // - "........15.......16\n"
          ".........5........6\n.........7........8\n.........9",
          ".......10\n........11........12........13........14",
          "........15.......16\n"))
    Try(runTest())  // Warm-up
    runTest()

  "InternalJob OrderStdoutWritten event compacting" in:
    def runTest() =
      testExecutable(
        TestInternalJob.executable(),
        Seq(
          "A\n",
          "B-1\n" + "B-2\n" + "B-3\n" + "B-4\n",
          "C\n",
          "D-1\n" + "D-2\n" + "D-3\n",
          ".........1........2\n.........3........4\n",
          // The InternalJob delivers the strings as is (not prepackaged in chunked)
          // - ".........5........6\n"
          // - ".........7........8\n"
          // - ".........9.......10\n"
          // - "........11........12........13........14........15.......16\n"
          ".........5........6\n.........7........8\n",
          // Break because Observable.buffer limit reached
          ".........9.......10\n",
          "........11........12........13........14........15",
          ".......16\n"))
    Try(runTest())  // Warm-up
    runTest()

  private def testExecutable(executable: Executable, expectedChunks: Seq[String]): Unit =
    testWithWorkflow(
      Workflow.of(Execute(WorkflowJob(agentPath, executable))),
      expectedChunks)

  private def testWithWorkflow(
    anonymousWorkflow: Workflow,
    expectedChunks: Seq[String])
  : Unit =
    val events = runWithWorkflow(anonymousWorkflow)
    val chunks = events.collect { case OrderStdoutWritten(chunk) => chunk }
    assert(chunks == expectedChunks)

  private def runWithWorkflow(
    anonymousWorkflow: Workflow,
    orderArguments: Map[String, Value] = Map.empty)
  : Seq[OrderEvent] =
    testPrintAndParse(anonymousWorkflow)

    val versionId = versionIdIterator.next()
    val workflow = anonymousWorkflow.withId(workflowPathIterator.next() ~ versionId)
    val order = FreshOrder(orderIdIterator.next(), workflow.path, arguments = orderArguments)
    directoryProvider.updateVersionedItems(controller, versionId, Seq(workflow))

    controller.runOrder(order).map(_.value)

  private def testPrintAndParse(anonymousWorkflow: Workflow): Unit =
    val workflowNotation = WorkflowPrinter.print(anonymousWorkflow.withoutSource)
    val reparsedWorkflow = WorkflowParser.parse(workflowNotation).map(_.withoutSource)
    logger.debug(workflowNotation)
    assert(reparsedWorkflow == Right(anonymousWorkflow.withoutSource))

object StdoutTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val chunkSize = 50
  private val delay = 200.ms
  private val shortDelay = 10.ms
  private val longDelay = delay + 400.ms

  private def sleepCommand(delay: FiniteDuration) =
    if isWindows then s"SLEEP ${delay.toDecimalString}\n"  // TODO Windows
    else s"sleep ${delay.toDecimalString}\n"

  private final class TestInternalJob extends InternalJob:
    def toOrderProcess(step: Step) =
      import Task.sleep
      OrderProcess(
        step.send(Stdout, "A\n") >>
          sleep(longDelay) >>
          step.send(Stdout, "B-1\n") >>
          step.send(Stdout, "B-2\n") >>
          sleep(shortDelay) >>
          step.send(Stdout, "B-3\n") >>
          step.send(Stdout, "B-4\n") >>
          sleep(longDelay) >>
          step.send(Stdout, "C\n") >>
          sleep(longDelay) >>
          step.send(Stdout, "D-1\n") >>
          step.send(Stdout, "D-2\n") >>
          sleep(shortDelay) >>
          step.send(Stdout, "D-3\n") >>
          sleep(longDelay) >>
          step.send(Stdout, ".........1........2\n") >>
          step.send(Stdout, ".........3........4\n") >>
          sleep(longDelay) >>
          step.send(Stdout, ".........5........6\n") >>
          step.send(Stdout, ".........7........8\n") >>
          step.send(Stdout, ".........9.......10\n") >>
          step.send(Stdout, "........11........12........13........14........15.......16\n") >>
          Task.pure(Outcome.succeeded))
  private object TestInternalJob extends InternalJob.Companion[TestInternalJob]
