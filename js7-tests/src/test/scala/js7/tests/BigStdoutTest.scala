package js7.tests

import cats.effect.IO
import fs2.Stream
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEither}
import js7.data.agent.AgentPath
import js7.data.event.EventRequest
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.BigStdoutTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.concurrent.duration.Deadline

final class BigStdoutTest extends OurAsyncTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.order.stdout-stderr.delay = 999s # No delay should occur in this test!
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "Speed test big stdout" in:
    val megabytes = sys.props.get("test.speed").map(_.toInt).getOrElse(10)
    logger.info(s"${megabytes}MB")
    assert(megabytes >= 1)

    val workflow = Workflow(WorkflowPath("BIG-STDOUT"), Seq:
      Execute(WorkflowJob(agentPath,
        ShellScriptExecutable:
          // TODO Surrogat 🌈 am Ende eines Chunks wird zerschnitten und setzt im nächsten Chunk
          //  fort. Damit ist es kaputt.
          s"""#!/usr/bin/env bash
             |set -euo pipefail
             |line="╳ ++++ A HUNDRED BYTES LONG LINE +++ A HUNDRED BYTES LONG LINE +++ A HUNDRED BYTES LONG LINE ++++"
             |line="$$line$$line$$line$$line$$line$$line$$line$$line$$line$$line"
             |for i in {0..${megabytes - 1}}; do
             |  # Write 1MB in each iteration
             |  # 🌈 is a surrogate — We still not support surrogates, so we take '╳'
             |  # line has about 1000 bytes, no write 1MB
             |  for j in {000..999}; do
             |    # Workaround for old bash version (like in MacOS)
             |    #j2="00$$j"
             |    #j2="$${j2:$$(($${#j2} - 3))}"
             |    echo "$$i.$$j MB: $$line"
             |  done
             |done
             |""".stripMargin)))
    withItem(workflow): workflow =>
      val orderId = OrderId("BIG-STDOUT")
      val since = Deadline.now
      var eventCount, charCount = 0L
      val eventId = eventWatch.lastAddedEventId

      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      val line1K =
        "╳ ++++ A HUNDRED BYTES LONG LINE +++ A HUNDRED BYTES LONG LINE +++ A HUNDRED BYTES LONG LINE ++++"
          * 10 + "\n"
      eventWatch
        .stream(
          EventRequest.singleClass[OrderEvent](after = eventId, timeout = None),
          _.key == orderId)
        .map(_.value.event)
        .takeWhile(e => !e.isInstanceOf[OrderTerminated])
        .collect:
          case OrderStdoutWritten(string) =>
            eventCount += 1
            charCount += string.length
            string
        .through(fs2.text.lines)
        .zipAll(
          Stream.iterable(0 until megabytes).flatMap: i =>
            Stream.iterable(0 until 1000).map: j =>
              f"$i.$j%03d MB: $line1K"
          .through(fs2.text.lines))(
          "(NO MORE OUTPUT)", "(TOO MUCH OUTPUT)")
        .foreach/*parEvalMapUnorderedUnbounded*/: stringPair =>
          IO(assert(stringPair._1 == stringPair._2))
        .compile
        .drain
        .guarantee:
          IO:
            val elapsed = since.elapsed
            val byteCount = charCount // We estimate a character yields one byte
            logger.info(s"🔵🔵🔵 ${bytesPerSecondString(elapsed, byteCount)}"
              + " · " + itemsPerSecondString(elapsed, eventCount, "StdoutWrittenEvents")
              + ((eventCount > 0) ?? s" · ~${charCount / eventCount} characters/event"))
        .as(succeed)


object BigStdoutTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
