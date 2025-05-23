package js7.tests

import cats.instances.vector.*
import cats.syntax.traverse.*
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.AnswerOrderPrompt
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.value.{ListValue, StringValue}
import js7.data.workflow.instructions.{ForkList, Prompt}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ForkListRecoveryTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.DirectoryProviderForScalaTest
import cats.effect.unsafe.IORuntime

final class ForkListRecoveryTest extends OurTestSuite, DirectoryProviderForScalaTest:

  private given IORuntime = ioRuntime

  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  override protected def agentPaths = Seq(agentPath)

  override protected def items = Seq(workflow)

  "Fork" in:
    directoryProvider.run { (controller, _) =>
      controller.addOrderBlocking(newOrder(parentOrderId, workflow.path, n))
      controller.eventWatch.await[OrderPrompted](_.key == childOrderIds.head)
    }

  "Join" in:
    directoryProvider.run { (controller, _) =>
      childOrderIds.toVector
        .traverse(childOrderId =>
          controller.eventWatch
            .awaitAsync[OrderPrompted](_.key ==  childOrderId)
            .*>(controller.api.executeCommand(AnswerOrderPrompt(childOrderId))))
        .await(99.s)
          .combineProblems
          .orThrow
      assert(controller.eventWatch.await[OrderTerminated](_.key == parentOrderId)
        .head.value.event == OrderFinished())
      controller.eventWatch.await[OrderDeleted](_.key == parentOrderId)

      assert(controller.eventWatch.eventsByKey[OrderEvent](parentOrderId) == Seq(
        OrderAdded(
          workflow.id,
          Map("children" -> ListValue(Seq(
            StringValue("CHILD-1"),
            StringValue("CHILD-2"),
            StringValue("CHILD-3")))),
          deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderForked(Vector(
          Order.Forked.Child(
            childOrderIds(0),
            Map(
              "id" -> StringValue("CHILD-1"))),
          Order.Forked.Child(
            childOrderIds(1),
            Map(
              "id" -> StringValue("CHILD-2"))),
          Order.Forked.Child(
            childOrderIds(2),
            Map(
              "id" -> StringValue("CHILD-3"))))),
        OrderDetachable,
        OrderDetached,
        OrderJoined(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

      for orderId <- childOrderIds do
        assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(0) / "fork" % 1),
          OrderDetachable,
          OrderDetached,
          OrderPrompted(StringValue("QUESTION")),
          OrderPromptAnswered(),
          OrderMoved(Position(0) / "fork" % 2)))
    }


object ForkListRecoveryTest:
  private val n = 3
  private val parentOrderId = OrderId("PARENT")
  private val childOrderIds = for i <- 1 to 3 yield parentOrderId / s"CHILD-$i"
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val workflow = Workflow(
    WorkflowPath("AT-CONTROLLER-WORKFLOW") ~ "INITIAL",
    Vector(
      ForkList(
        expr("$children"),
        exprFunction("id => $id"),
        exprFunction("id => { id: $id }"),
        Workflow.of(
          EmptyJob.execute(agentPath),
          Prompt(expr("'QUESTION'"))))))

  private def newOrder(orderId: OrderId, workflowPath: WorkflowPath, n: Int) =
    FreshOrder(
      orderId,
      workflowPath,
      Map("children" -> ListValue(for i <- 1 to n yield StringValue(s"CHILD-$i"))),
      deleteWhenTerminated = true)
