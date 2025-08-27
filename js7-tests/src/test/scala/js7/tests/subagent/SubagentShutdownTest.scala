package js7.tests.subagent

import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.isSubtypeOf
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentCommand
import js7.data.subagent.SubagentItemStateEvent.SubagentShutdown
import js7.data.value.Value.convenience.given
import js7.data.value.expression.Expression.convenience.given
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.{Execute, If, StickySubagent}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.language.implicitConversions
import scala.util.Random

final class SubagentShutdownTest extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(bareSubagentItem)

  private val localSubagentId = toLocalSubagentId(agentPath)

  "Test" in:
    val n = 10
    val sleep = if isIntelliJIdea then 5.s else 1.s
    val orderIds = (1 to n).map(i => OrderId(s"ORDER-$i"))

    enableSubagents(localSubagentId -> false, bareSubagentId -> true)

    runSubagent(bareSubagentItem): subagent =>
      withItem(
        Workflow.of:
          StickySubagent(agentPath):
            If(true):
              Workflow.of(
                Execute.shellScript(agentPath, processLimit = n):
                  ":",
                Execute.shellScript(agentPath,
                  defaultArguments = Map("sleepArg" -> expr"$$sleep"),
                  env = Map("SLEEP" -> expr"$$sleepArg"),
                  processLimit = n
                ):
                  """echo STARTED
                    |sleep $SLEEP
                    |""".stripMargin)
      ): workflow =>
        controller.api.addOrders:
          orderIds.map: orderId =>
            FreshOrder(orderId, workflow.path,
              Map("sleep" -> Random.nextLong(1 + sleep.toMillis).ms.toBigDecimalSeconds),
              deleteWhenTerminated = true)
        .await(99.s).orThrow

        for orderId <- orderIds do
          controller.eventWatch.awaitKey[OrderProcessed](orderId)
          controller.eventWatch.awaitKey[OrderStdoutWritten](orderId)

        subagent.executeCommandForTest:
          SubagentCommand.ShutDown()
        .await(99.s).orThrow
        controller.awaitNextKey[SubagentShutdown](bareSubagentId)

        for orderId <- orderIds do
          val events = controller.eventWatch.awaitKey[OrderTerminated](orderId).map(_.value)
          assert(events.exists(_.isSubtypeOf[OrderFinished]))


object SubagentShutdownTest:
  private val logger = Logger[this.type]
