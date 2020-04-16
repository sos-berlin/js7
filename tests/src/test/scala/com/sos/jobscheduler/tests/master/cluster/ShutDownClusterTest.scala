package com.sos.jobscheduler.tests.master.cluster

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.data.Problems.PassiveClusterNodeShutdownNotAllowedProblem
import com.sos.jobscheduler.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterFailedOver, ClusterPassiveLost, ClusterSwitchedOver}
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
        backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            primaryMaster.eventWatch.await[ClusterCoupled]()

            primaryMaster.executeCommandAsSystemUser(ShutDown())
              .await(99.s).orThrow
            primaryMaster.terminated.await(99.s)
          }

          val activeShutDown = backupMaster.eventWatch.await[ClusterActiveNodeShutDown](after = EventId.BeforeFirst).head.eventId

          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            val activeRestarted = primaryMaster.eventWatch.await[ClusterActiveNodeRestarted](
              after = activeShutDown).head.eventId
            primaryMaster.eventWatch.await[ClusterCoupled](after = activeRestarted)
          }
        }
      }
    }

    "ShutDown active node with switchcover" in {
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
        backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
          primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
            primaryMaster.eventWatch.await[ClusterCoupled]()

            primaryMaster.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).orThrow
            primaryMaster.terminated.await(99.s)
            backupMaster.eventWatch.await[ClusterFailedOver]()
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

    "ShutDown passive node with switchcover or failover is rejected" in {
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

