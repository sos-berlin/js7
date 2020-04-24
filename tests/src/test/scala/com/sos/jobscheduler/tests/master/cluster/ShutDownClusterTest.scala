package com.sos.jobscheduler.tests.master.cluster

import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.data.Problems.PassiveClusterNodeShutdownNotAllowedProblem
import com.sos.jobscheduler.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterFailedOver, ClusterPassiveLost, ClusterSwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.cluster.ClusterState.{Coupled, FailedOver}
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.master.data.MasterCommand.ShutDown
import com.sos.jobscheduler.master.data.MasterCommand.ShutDown.ClusterAction
import monix.execution.Scheduler.Implicits.global

final class ShutDownClusterTest extends MasterClusterTester
{
  override protected def removeObsoleteJournalFiles = false
  private lazy val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)

  "ShutDown active node" - {
    "ShutDown primary node only (no switchover)" in {
      withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        val idToUri = Map(
          primaryId -> Uri(s"http://127.0.0.1:$primaryHttpPort"),
          backupId -> Uri(s"http://127.0.0.1:$backupHttpPort"))

        backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
          backupMaster.httpApi.login(Some(UserAndPassword(UserId("TEST"), SecretString("TEST-PASSWORD")))).await(99.s)

          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            primaryMaster.eventWatch.await[ClusterCoupled]()
            primaryMaster.httpApi.login(Some(UserAndPassword(UserId("TEST"), SecretString("TEST-PASSWORD")))).await(99.s)
            val coupled = primaryMaster.httpApi.clusterState.await(99.s)
            assert(coupled.isInstanceOf[ClusterState.Coupled])

            primaryMaster.executeCommandAsSystemUser(ShutDown())
              .await(99.s).orThrow
            primaryMaster.terminated.await(99.s)
          }

          val activeNodeShutDown = backupMaster.eventWatch.await[ClusterActiveNodeShutDown](after = EventId.BeforeFirst).head.eventId

          assert(backupMaster.httpApi.clusterState.await(99.s) == ClusterState.ActiveShutDown(idToUri, primaryId))

          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            val activeRestarted = primaryMaster.eventWatch.await[ClusterActiveNodeRestarted](
              after = activeNodeShutDown).head.eventId
            primaryMaster.eventWatch.await[ClusterCoupled](after = activeRestarted)
            assert(primaryMaster.clusterState.await(99.s) == Coupled(idToUri, primaryId))
          }
        }
      }
    }

    "ShutDown active node with switchover" in {
      withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            primaryMaster.eventWatch.await[ClusterCoupled]()

            primaryMaster.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Switchover)))
              .await(99.s).orThrow
            primaryMaster.eventWatch.await[ClusterSwitchedOver]()
            primaryMaster.terminated.await(99.s)
            backupMaster.eventWatch.await[ClusterSwitchedOver]()
          }
        }
      }
    }

    "ShutDown active node with failover (for testing)" in {
      withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        val idToUri = Map(
          primaryId -> Uri(s"http://127.0.0.1:$primaryHttpPort"),
          backupId -> Uri(s"http://127.0.0.1:$backupHttpPort"))
        backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            primaryMaster.eventWatch.await[ClusterCoupled]()

            primaryMaster.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).orThrow
            primaryMaster.terminated.await(99.s)
            backupMaster.eventWatch.await[ClusterFailedOver]()
            waitForCondition(3.s, 10.ms)(backupMaster.clusterState.await(99.s).isInstanceOf[FailedOver])
            assert(backupMaster.clusterState.await(99.s).asInstanceOf[FailedOver].activeId == backupId)
          }
          primary.runMaster(httpPort = Some(primaryHttpPort), dontWaitUntilReady = true) { primaryMaster =>
            // Restarted Primary should have become passive
            primaryMaster.eventWatch.await[ClusterCoupled](after = primaryMaster.eventWatch.lastFileTornEventId)
            waitForCondition(3.s, 10.ms)(primaryMaster.clusterState.await(99.s).isInstanceOf[Coupled])
            assert(primaryMaster.clusterState.await(99.s) == backupMaster.clusterState.await(99.s))
            assert(primaryMaster.clusterState.await(99.s) == Coupled(idToUri, backupId))

            backupMaster.executeCommandAsSystemUser(ShutDown()).await(99.s).orThrow
            backupMaster.terminated await 99.s
          }
        }
      }
    }

    "ShutDown active node with failover requested (for testing), but immediate restart of the shut down node" in {
      withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        val idToUri = Map(
          primaryId -> Uri(s"http://127.0.0.1:$primaryHttpPort"),
          backupId -> Uri(s"http://127.0.0.1:$backupHttpPort"))
        backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            primaryMaster.eventWatch.await[ClusterCoupled]()

            primaryMaster.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).orThrow
            primaryMaster.terminated.await(99.s)
          }
          primary.runMaster(httpPort = Some(primaryHttpPort), dontWaitUntilReady = true) { primaryMaster =>
            assert(primaryMaster.clusterState.await(99.s) == backupMaster.clusterState.await(99.s))
            assert(primaryMaster.clusterState.await(99.s) == Coupled(idToUri, primaryId))

            backupMaster.executeCommandAsSystemUser(ShutDown()).await(99.s).orThrow
            backupMaster.terminated await 99.s
          }
        }
      }
    }

    //"CompleteShutDown" in {
    //  val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    //  withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
    //    backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
    //      primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
    //        primaryMaster.eventWatch.await[ClusterCoupled]()
    //
    //        primaryMaster.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.CompleteShutdown)))
    //          .await(99.s).orThrow
    //        primaryMaster.terminated.await(99.s)
    //        backupMaster.terminated.await(99.s)
    //      }
    //    }
    //
    //    backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { _ =>
    //      primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
    //        val clusterCoupled = primaryMaster.eventWatch.await[ClusterCoupled]().head.value.event
    //        assert(clusterCoupled.activeId == ClusterNodeId("Primary"))
    //      }
    //    }
    //  }
    //}
  }

  "ShutDown passive node" - {
    "ShutDown passive node only (no switchover)" in {
      withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            primaryMaster.eventWatch.await[ClusterCoupled]()

            backupMaster.executeCommandAsSystemUser(ShutDown()).await(99.s).orThrow
            backupMaster.terminated.await(99.s)
            primaryMaster.eventWatch.await[ClusterPassiveLost]()
          }
        }
      }
    }

    "ShutDown passive node with switchover or failover is rejected" in {
      withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
        backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            primaryMaster.eventWatch.await[ClusterCoupled]()

            backupMaster.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Switchover)))
              .await(99.s).left.map(_.codeOption contains PassiveClusterNodeShutdownNotAllowedProblem.code)
            backupMaster.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).left.map(_.codeOption contains PassiveClusterNodeShutdownNotAllowedProblem.code)
          }
        }
      }
    }
  }
}

