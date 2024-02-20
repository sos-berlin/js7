package js7.tests.cluster.controller

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.instances.vector.*
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
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
import scala.concurrent.duration.Deadline.now

final class SpeedControllerClusterTest extends OurTestSuite, ControllerClusterForScalaTest:

  private given IORuntime = ioRuntime

  val items = Seq(workflow)

  for n <- sys.props.get("test.speed"/*try 1000*/).map(_.toInt) do
    "Speed test" in:
      withControllerAndBackup() { (primary, _, backup, _, _) =>
        backup.runController(dontWaitUntilReady = true) { _ =>
          primary.runController() { primaryController =>
            primaryController.eventWatch.await[ClusterEvent.ClusterCoupled]()

            val orderIdIterator = Iterator.from(1).map(i => OrderId(i.toString))

            def cycle = IO.defer:
              val orderId = orderIdIterator.synchronized(orderIdIterator.next())
              val order = FreshOrder(orderId, workflow.path)
              // 3 acks:
              for
                _ <- primaryController.api.addOrder(order).map(_.orThrow)
                _ <- primaryController.api.executeCommand(CancelOrders(Seq(order.id)))
                  .map(_.orThrow)
                _ <- primaryController.api.executeCommand(DeleteOrdersWhenTerminated(Seq(order.id)))
                  .map(_.orThrow)
              yield ()

            def run() =
              val concurrency = sys.runtime.availableProcessors min 30 /*number of connections*/
              IO.parTraverseN(concurrency)(Vector.fill(1000)(())): _ =>
                cycle
              .await(99.s)

            run() // Warm up

            val t = now
            run()
            info(Stopwatch.itemsPerSecondString(t.elapsed, 3 * n, "acks"))
          }
        }
      }


object SpeedControllerClusterTest:
  private val workflow = Workflow(WorkflowPath("SPEED") ~ "INITIAL",
    Seq(Prompt(StringConstant(""))))
