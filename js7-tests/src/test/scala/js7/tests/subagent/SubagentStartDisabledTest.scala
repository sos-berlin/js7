package js7.tests.subagent

import cats.syntax.option.*
import cats.effect.unsafe.IORuntime
import fs2.Stream
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentStartDisabledTest.*
import js7.tests.testenv.DirectoryProviderForScalaTest
import org.scalatest.Assertion

/** Test for JS-2114 . */
final class SubagentStartDisabledTest extends OurAsyncTestSuite with DirectoryProviderForScalaTest
{
  private given IORuntime = ioRuntime

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.command-error-delay = 100ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected def items = Seq(workflow)

  private val localSubagentItem = directoryProvider.subagentItems.head

  private val orderIds = Iterator(OrderId("🟦"), OrderId("🟧"))

  "Start Director with disabled Subagent, then enable" in
    testDisabledSubagentAtAgentState(restartAgent = false)

  "Restart Director with disabled Subagent, enable" in
    testDisabledSubagentAtAgentState(restartAgent = true)

  private def testDisabledSubagentAtAgentState(restartAgent: Boolean): Assertion = {
    val controller = directoryProvider
      .newController(httpPort = findFreeTcpPort().some)

    try
      var agent = directoryProvider.startAgent(agentPath).await(99.s)
      val eventWatch = controller.eventWatch

      def enableOrDisableSubagent(enabled: Boolean): Unit =
        eventWatch.expect[ItemAttached](_.event.key == localSubagentItem.id):
          controller.api
            .updateItems(Stream.emit(
              AddOrChangeSimple(
                localSubagentItem.withRevision(None).copy(
                  disabled = !enabled))))
            .await(99.s).orThrow

      try
        enableOrDisableSubagent(enabled = false)

        if restartAgent then {
          agent.stop.await(99.s)

          agent = directoryProvider.startAgent(agentPath).await(99.s)
        }

        enableOrDisableSubagent(enabled = true)

        val orderId = orderIds.next()
        controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        eventWatch.await[OrderProcessingStarted](_.key == orderId)
        eventWatch.await[OrderFinished](_.key == orderId)
        succeed
      finally
        agent.stop.await(99.s)
    finally
      controller.stop.await(99.s)
  }
}

object SubagentStartDisabledTest
{
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW"),
    Seq(
      EmptyJob.execute(agentPath, processLimit = 100)))
}
