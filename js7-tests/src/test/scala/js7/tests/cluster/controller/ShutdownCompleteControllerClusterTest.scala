package js7.tests.cluster.controller

final class ShutdownCompleteControllerClusterTest {
  //"CompleteShutDown" in {
  //  val primaryControllerPort :: backupControllerPort :: Nil = findFreeTcpPorts(2)
  //  withControllerAndBackup() { (primary, backup) =>
  //    backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
  //      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
  //        primaryController.eventWatch.await[ClusterCoupled]()
  //
  //        primaryController.api.executeCommand(ShutDown(clusterAction = Some(ClusterAction.CompleteShutdown)))
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
