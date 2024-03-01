package js7.tests.cluster.controller

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.DurationRichInt
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.cluster.{ClusterState, ClusterWatchId}
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.cluster.controller.ClusterWatchChangeWhileNodesRestartsTest.*
import js7.tests.cluster.controller.ControllerClusterTester.TestWorkflow

// Both cluster nodes restart after hard stop while ClusterWatch changes
final class ClusterWatchChangeWhileNodesRestartsTest extends ControllerClusterTester:

  override protected def primaryControllerConfig = config"""
      # Short timeout because something blocks web server shutdown occasionally
      js7.web.server.shutdown-timeout = 500ms
      js7.web.server.shutdown-delay = 500ms
      """.withFallback(super.primaryControllerConfig)

  "Start ClusterWatch first" in:
    withControllerAndBackup(suppressClusterWatch = true) { (primary, _, backup, _, _) =>
      var eventId = 0L
      backup.runController(dontWaitUntilReady = true) { _ =>
        withClusterWatchService() { (_, _) =>
          primary.runController() { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()
            primaryController.runOrder(FreshOrder(OrderId("FIRST"), TestWorkflow.path))
            eventId = primaryController.eventWatch.lastAddedEventId
            // Shut down without ClusterActiveNodeShutDown
            primaryController.terminate(clusterAction = Some(ShutDown.ClusterAction.Failover)).await(99.s)
          }
        }
      }

      assert(primary.controllerEnv.recoverState.clusterState.isInstanceOf[ClusterState.Coupled])
      logger.info(s"\n\n${"━" * 80}\n")

      backup.runController(dontWaitUntilReady = true) { _ =>
        withClusterWatchService(clusterWatchId = ClusterWatchId("CLUSTER-WATCH-2")) { (_, _) =>
          primary.runController() { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled](after = eventId)
            primaryController.runOrder(FreshOrder(OrderId("SECOND"), TestWorkflow.path))
          }
        }
      }
    }


object ClusterWatchChangeWhileNodesRestartsTest:
  private val logger = Logger[this.type]
