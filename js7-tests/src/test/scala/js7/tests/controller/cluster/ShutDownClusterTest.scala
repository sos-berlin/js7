package js7.tests.controller.cluster

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.data.ControllerCommand.ShutDown
import js7.controller.data.ControllerCommand.ShutDown.ClusterAction
import js7.data.Problems.PassiveClusterNodeShutdownNotAllowedProblem
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterFailedOver, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.event.EventId
import monix.execution.Scheduler.Implicits.global

final class ShutDownClusterTest extends ControllerClusterTester
{
  override protected def removeObsoleteJournalFiles = false
  private lazy val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)

  "ShutDown active node" - {
    "ShutDown primary node only (no switchover)" in {
      withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        val idToUri = Map(
          primaryId -> Uri(s"http://127.0.0.1:$primaryHttpPort"),
          backupId -> Uri(s"http://127.0.0.1:$backupHttpPort"))

        backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupController =>
          backupController.httpApi.login_(Some(UserAndPassword(UserId("TEST"), SecretString("TEST-PASSWORD")))).await(99.s)

          primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()
            primaryController.httpApi.login_(Some(UserAndPassword(UserId("TEST"), SecretString("TEST-PASSWORD")))).await(99.s)
            val coupled = primaryController.httpApi.clusterState.await(99.s)
            assert(coupled.isInstanceOf[ClusterState.Coupled])

            primaryController.executeCommandAsSystemUser(ShutDown())
              .await(99.s).orThrow
            primaryController.terminated.await(99.s)
          }

          val activeNodeShutDown = backupController.eventWatch.await[ClusterActiveNodeShutDown](after = EventId.BeforeFirst).head.eventId

          assert(backupController.httpApi.clusterState.await(99.s) == ClusterState.CoupledActiveShutDown(idToUri, primaryId))

          primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
            val activeRestarted = primaryController.eventWatch.await[ClusterActiveNodeRestarted](
              after = activeNodeShutDown).head.eventId
            primaryController.eventWatch.await[ClusterCoupled](after = activeRestarted)
            assert(primaryController.clusterState.await(99.s) == Coupled(idToUri, primaryId))
          }
        }
      }
    }

    "ShutDown active node with switchover" in {
      withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()

            primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Switchover)))
              .await(99.s).orThrow
            backupController.eventWatch.await[ClusterSwitchedOver]()
            primaryController.terminated.await(99.s)
          }
        }
      }
    }

    "ShutDown active node with failover (for testing)" in {
      withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        val idToUri = Map(
          primaryId -> Uri(s"http://127.0.0.1:$primaryHttpPort"),
          backupId -> Uri(s"http://127.0.0.1:$backupHttpPort"))
        backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()

            primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).orThrow
            primaryController.terminated.await(99.s)
            backupController.eventWatch.await[ClusterFailedOver]()
            waitForCondition(3.s, 10.ms)(backupController.clusterState.await(99.s).isInstanceOf[FailedOver])
            assert(backupController.clusterState.await(99.s).asInstanceOf[FailedOver].activeId == backupId)
          }
          primary.runController(httpPort = Some(primaryHttpPort), dontWaitUntilReady = true) { primaryController =>
            // Restarted Primary should have become passive
            primaryController.eventWatch.await[ClusterCoupled](after = primaryController.eventWatch.lastFileTornEventId)
            waitForCondition(3.s, 10.ms)(primaryController.clusterState.await(99.s).isInstanceOf[Coupled])
            assert(primaryController.clusterState.await(99.s) == backupController.clusterState.await(99.s))
            assert(primaryController.clusterState.await(99.s) == Coupled(idToUri, backupId))

            backupController.executeCommandAsSystemUser(ShutDown()).await(99.s).orThrow
            backupController.terminated await 99.s
          }
        }
      }
    }

    "ShutDown active node with failover requested (for testing), but immediate restart of the shut down node" in {
      withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        val idToUri = Map(
          primaryId -> Uri(s"http://127.0.0.1:$primaryHttpPort"),
          backupId -> Uri(s"http://127.0.0.1:$backupHttpPort"))
        backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()

            primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).orThrow
            primaryController.terminated.await(99.s)
          }
          primary.runController(httpPort = Some(primaryHttpPort), dontWaitUntilReady = true) { primaryController =>
            assert(primaryController.clusterState.await(99.s) == backupController.clusterState.await(99.s))
            assert(primaryController.clusterState.await(99.s) == Coupled(idToUri, primaryId))

            // FIXME When Primary Controller is shutting down before started (no ControllerOrderKeeper)
            //  while the Backup Controller is shutting down and times out the acknowledgement request,
            //  Cluster asks the JournalActor about ClusterState for issuing a ClusterPassiveLost event.
            //  But JournalActor does not answert if not started.
            primaryController.waitUntilReady()

            backupController.executeCommandAsSystemUser(ShutDown()).await(99.s).orThrow
            backupController.terminated await 99.s
          }
        }
      }
    }

    //"CompleteShutDown" in {
    //  val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    //  withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
    //    backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupController =>
    //      primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
    //        primaryController.eventWatch.await[ClusterCoupled]()
    //
    //        primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.CompleteShutdown)))
    //          .await(99.s).orThrow
    //        primaryController.terminated.await(99.s)
    //        backupController.terminated.await(99.s)
    //      }
    //    }
    //
    //    backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { _ =>
    //      primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
    //        val clusterCoupled = primaryController.eventWatch.await[ClusterCoupled]().head.value.event
    //        assert(clusterCoupled.activeId == NodeId("Primary-Controller"))
    //      }
    //    }
    //  }
    //}
  }

  "ShutDown passive node" - {
    "ShutDown passive node only (no switchover)" in {
      withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()

            backupController.executeCommandAsSystemUser(ShutDown()).await(99.s).orThrow
            backupController.terminated.await(99.s)
            primaryController.eventWatch.await[ClusterPassiveLost]()
          }
        }
      }
    }

    "ShutDown passive node with switchover or failover is rejected" in {
      withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()

            backupController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Switchover)))
              .await(99.s).left.map(_ is PassiveClusterNodeShutdownNotAllowedProblem)
            backupController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).left.map(_ is PassiveClusterNodeShutdownNotAllowedProblem)
          }
        }
      }
    }
  }
}

