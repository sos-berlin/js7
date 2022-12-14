package js7.tests.controller.cluster

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.*
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.data.Problems.PassiveClusterNodeShutdownNotAllowedProblem
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterFailedOver, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.controller.ControllerCommand.ShutDown.ClusterAction
import js7.data.event.EventId
import monix.execution.Scheduler.Implicits.traced

final class ShutDownClusterWithLegacyClusterWatchTest extends ShutDownClusterTest {
  override protected val useLegacyServiceClusterWatch = true
}

class ShutDownClusterTest extends ControllerClusterTester
{
  override protected def removeObsoleteJournalFiles = false

  "ShutDown active node" - {
    "ShutDown primary node only (no switchover)" in {
      withControllerAndBackup() { (primary, backup, clusterSetting) =>
        backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
          backupController.httpApi.login_(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))).await(99.s)

          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()
            primaryController.httpApi.login_(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))).await(99.s)
            assert(primaryController.httpApi.clusterState.await(99.s) == Right(Coupled(clusterSetting)))

            primaryController.executeCommandAsSystemUser(ShutDown())
              .await(99.s).orThrow
            primaryController.terminated.await(99.s)
          }

          val activeNodeShutDown = backupController.eventWatch.await[ClusterActiveNodeShutDown](after = EventId.BeforeFirst).head.eventId

          assert(backupController.httpApi.clusterState.await(99.s) == Right(ClusterState.ActiveShutDown(clusterSetting)))

          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
            val activeRestarted = primaryController.eventWatch.await[ClusterActiveNodeRestarted](
              after = activeNodeShutDown).head.eventId
            primaryController.eventWatch.await[ClusterCoupled](after = activeRestarted)
            // TODO EventWatch delivers event after it has been written but befor ack!
            waitForCondition(2.s, 10.ms)(primaryController.clusterState.await(99.s) == Coupled(clusterSetting))
            assert(primaryController.clusterState.await(99.s) == Coupled(clusterSetting))
          }
        }
      }
    }

    "ShutDown active node with switchover" in {
      withControllerAndBackup() { (primary, backup, _) =>
        backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
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
      withControllerAndBackup() { (primary, backup, clusterSetting) =>
        backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()

            primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).orThrow
            primaryController.terminated.await(99.s)
            backupController.eventWatch.await[ClusterFailedOver]()
            waitForCondition(3.s, 10.ms)(backupController.clusterState.await(99.s).isInstanceOf[FailedOver])
            assert(backupController.clusterState.await(99.s).asInstanceOf[FailedOver].activeId == backupId)
          }
          primary.runController(httpPort = Some(primaryControllerPort), dontWaitUntilReady = true) { primaryController =>
            // Restarted Primary should have become passive
            primaryController.eventWatch.await[ClusterCoupled](after = primaryController.eventWatch.lastFileEventId)
            waitForCondition(3.s, 10.ms)(primaryController.clusterState.await(99.s).isInstanceOf[Coupled])
            assert(primaryController.clusterState.await(99.s) == backupController.clusterState.await(99.s))
            assert(primaryController.clusterState.await(99.s) == Coupled(clusterSetting.copy(activeId = backupId)))

            backupController.executeCommandAsSystemUser(ShutDown()).await(99.s).orThrow
            backupController.terminated await 99.s
          }
        }
      }
    }

    "ShutDown active node with failover requested (for testing), then immediate restart of the shut down node" in {
      withControllerAndBackup() { (primary, backup, clusterSetting) =>
        backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()

            primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).orThrow
            primaryController.terminated.await(99.s)
          }
          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
            assert(primaryController.clusterState.await(99.s) == Coupled(clusterSetting))
            assert(primaryController.clusterState.await(99.s) == backupController.clusterState.await(99.s))

            // FIXME When Primary Controller is shutting down before started (no ControllerOrderKeeper)
            //  while the Backup Controller is shutting down and times out the acknowledgement request,
            //  Cluster asks the JournalActor about ClusterState for issuing a ClusterPassiveLost event.
            //  But JournalActor does not answer if not started.
            primaryController.waitUntilReady()

            backupController.executeCommandAsSystemUser(ShutDown()).await(99.s).orThrow
            backupController.terminated await 99.s
          }
        }
      }
    }

    //"CompleteShutDown" in {
    //  val primaryControllerPort :: backupControllerPort :: Nil = findFreeTcpPorts(2)
    //  withControllerAndBackup() { (primary, backup) =>
    //    backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
    //      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
    //        primaryController.eventWatch.await[ClusterCoupled]()
    //
    //        primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.CompleteShutdown)))
    //          .await(99.s).orThrow
    //        primaryController.terminated.await(99.s)
    //        backupController.terminated.await(99.s)
    //      }
    //    }
    //
    //    backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { _ =>
    //      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
    //        val clusterCoupled = primaryController.eventWatch.await[ClusterCoupled]().head.value.event
    //        assert(clusterCoupled.activeId == NodeId("PRIMARY))
    //      }
    //    }
    //  }
    //}
  }

  "ShutDown passive node" - {
    "ShutDown passive node only (no switchover)" in {
      withControllerAndBackup() { (primary, backup, _) =>
        backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()

            backupController.executeCommandAsSystemUser(ShutDown()).await(99.s).orThrow
            backupController.terminated.await(99.s)
            primaryController.eventWatch.await[ClusterPassiveLost]()
          }
        }
      }
    }

    "ShutDown passive node with switchover or failover is rejected" in {
      withControllerAndBackup() { (primary, backup, _) =>
        backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
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
