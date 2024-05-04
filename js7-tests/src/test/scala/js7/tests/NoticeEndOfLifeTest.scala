package js7.tests

import js7.base.configutils.Configs.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.board.{Board, BoardPath, BoardPathExpression, NoticeId}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.order.OrderEvent.{OrderFinished, OrderNoticesExpected}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.{ConsumeNotices, ExpectNotices, PostNotices}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.NoticeEndOfLifeTest.*
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}

final class NoticeEndOfLifeTest
  extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  override protected val controllerConfig = config"""
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)

  private val board = Board.joc(BoardPath("BOARD"), lifetime = 500.ms)
  private val postWorkflow = Workflow(WorkflowPath("POST-WORKFLOW"), Vector(
    PostNotices(Seq(board.path))))
  private val consumeWorkflow = Workflow(WorkflowPath("CONSUME-WORKFLOW"), Vector(
    ExpectNotices(BoardPathExpression.ExpectNotice(board.path)),
    ConsumeNotices(BoardPathExpression.ExpectNotice(board.path),
      Workflow.empty)))

  protected val items = Seq(board, postWorkflow, consumeWorkflow)

  "PostNotices instruction" - {
    "Until endOfLife, the Notice is consumable" in:
      val postOrderId = OrderId("#2023-06-22#🟦")
      val postEvents = controller.runOrder(FreshOrder(postOrderId, postWorkflow.path))
      assert(postEvents.last.value.isInstanceOf[OrderFinished])

      val consumeOrderId = OrderId("#2023-06-22#🔷")
      val consumeEvents = controller.runOrder(FreshOrder(consumeOrderId, consumeWorkflow.path))
      assert(!consumeEvents.map(_.value).exists(_.isInstanceOf[OrderNoticesExpected]))
      assert(consumeEvents.last.value.isInstanceOf[OrderFinished])

    "After endOfLife, the Notice vanishes" in:
      val postOrderId = OrderId("#2023-06-23#🟧")
      val postEvents = controller.runOrder(FreshOrder(postOrderId, postWorkflow.path))
      assert(postEvents.last.value.isInstanceOf[OrderFinished])

      sleep(600.ms)
      val consumeOrderId = OrderId("#2023-06-23#🔶")
      controller.addOrderBlocking(FreshOrder(consumeOrderId, consumeWorkflow.path))
      controller.eventWatch.await[OrderNoticesExpected](_.key == consumeOrderId)
      controller.api.executeCommand(CancelOrders(Set(consumeOrderId)))
        .await(99.s).orThrow
  }

  "PostNotice command" - {
    "Until endOfLife, the Notice is consumable" in:
      controller
        .api.executeCommand(ControllerCommand.PostNotice(board.path, NoticeId("2023-06-24")))
        .await(99.s).orThrow
      val consumeOrderId = OrderId("#2023-06-24#🔔")
      val events = controller.runOrder(FreshOrder(consumeOrderId, consumeWorkflow.path))
      assert(!events.map(_.value).exists(_.isInstanceOf[OrderNoticesExpected]))
      assert(events.last.value.isInstanceOf[OrderFinished])

    "After endOfLife, the Notice vanishes" in:
      controller
        .api.executeCommand(ControllerCommand.PostNotice(board.path, NoticeId("2023-06-24")))
        .await(99.s).orThrow
      sleep(600.ms)
      val consumeOrderId = OrderId("#2023-06-24#🔕")
      controller.addOrderBlocking(FreshOrder(consumeOrderId, consumeWorkflow.path))
      controller.eventWatch.await[OrderNoticesExpected](_.key == consumeOrderId)
      controller.api.executeCommand(CancelOrders(Set(consumeOrderId)))
        .await(99.s).orThrow
  }

  "PostNotice command with immediately expired endOfLife" - {
    "The Notice is immediately deleted" in:
      controller
        .api.executeCommand(ControllerCommand.PostNotice(board.path, NoticeId("2023-06-25"),
          endOfLife = Some(Timestamp.now)))
        .await(99.s).orThrow
      val consumeOrderId = OrderId("#2023-06-25#🟪")
      controller.addOrderBlocking(FreshOrder(consumeOrderId, consumeWorkflow.path))
      controller.eventWatch.await[OrderNoticesExpected](_.key == consumeOrderId)
      controller.api.executeCommand(CancelOrders(Set(consumeOrderId))).await(99.s).orThrow
  }


object NoticeEndOfLifeTest:

  private val agentPath = AgentPath("AGENT")
