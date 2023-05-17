package js7.tests.cluster.controller

import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.data.cluster.ClusterEvent
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteOrdersWhenTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.instructions.Prompt
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.cluster.controller.SpeedControllerClusterTest.*
import js7.tests.testenv.ControllerClusterForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration.Deadline.now

final class SpeedControllerClusterTest extends OurTestSuite with ControllerClusterForScalaTest
{
  val items = Seq(workflow)

  for (n <- sys.props.get("test.speed"/*try 1000*/).map(_.toInt)) {
    "Speed test" in {
      withControllerAndBackup() { (primary, _, backup, _, _) =>
        backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { _ =>
          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterEvent.ClusterCoupled]()

            val orderIdIterator = Iterator.from(1).map(i => OrderId(i.toString))

            def cycle = Task.defer {
              val orderId = orderIdIterator.synchronized(orderIdIterator.next())
              val order = FreshOrder(orderId, workflow.path)
              // 3 acks:
              for {
                _ <- primaryController.api.addOrder(order).map(_.orThrow)
                _ <- primaryController.api.executeCommand(CancelOrders(Seq(order.id)))
                  .map(_.orThrow)
                _ <- primaryController.api.executeCommand(DeleteOrdersWhenTerminated(Seq(order.id)))
                  .map(_.orThrow)
              } yield ()
            }
            // Warm up
            Task.parSequence((1 to 1000).map(_ => cycle)).await(99.s)

            val t = now
            Task.parSequence((1 to n).map(_ => cycle)).await(99.s)
            info(Stopwatch.itemsPerSecondString(t.elapsed, 3 * n, "acks"))
          }
        }
      }
    }
  }
}

object SpeedControllerClusterTest
{
  private val workflow = Workflow(WorkflowPath("SPEED") ~ "INITIAL",
    Seq(Prompt(StringConstant(""))))
}
