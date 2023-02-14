package js7.data_for_java.cluster

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterTiming}
import js7.data.event.JournalPosition
import js7.data.node.NodeId

/**
  * @author Joacim Zschimmer
  */
final class JClusterStateTest extends OurTestSuite
{
  private val setting = ClusterSetting(
    Map(
      NodeId("PRIMARY") -> Uri("https://PRIMARY"),
      NodeId("BACKUP") -> Uri("https://BACKUP")),
    activeId = NodeId("PRIMARY"),
    ClusterTiming(10.s, 20.s),
    clusterWatchId = None)

  "Empty" in {
    JClusterStateTester.testEmpty(JClusterState(ClusterState.Empty).asInstanceOf[JClusterState.Empty.type])
  }

  "NodesAppointed" in {
    val clusterState = ClusterState.NodesAppointed(setting)
    JClusterStateTester.testNodesAppointed(JClusterState(clusterState).asInstanceOf[JClusterState.NodesAppointed])
  }

  "HasNodes" in {
    val clusterState = ClusterState.Coupled(setting)
    JClusterStateTester.testHasNodes(JClusterState(clusterState).asInstanceOf[JClusterState.HasNodes])
  }

  "Coupled" in {
    val clusterState = ClusterState.Coupled(setting)
    JClusterStateTester.testCoupled(JClusterState(clusterState).asInstanceOf[JClusterState.Coupled])
    JClusterStateTester.testCoupledOrDecoupled(JClusterState(clusterState).asInstanceOf[JClusterState.CoupledOrDecoupled])
  }

  "PreparedToBeCoupled" in {
    val clusterState = ClusterState.PreparedToBeCoupled(setting)
    JClusterStateTester.testPreparedToBeCoupled(JClusterState(clusterState).asInstanceOf[JClusterState.PreparedToBeCoupled])
  }

  "Reset" in {
    val clusterState = ClusterState.PassiveLost(setting)
    JClusterStateTester.testDecoupled(JClusterState(clusterState).asInstanceOf[JClusterState.Decoupled])
    JClusterStateTester.testCoupledOrDecoupled(JClusterState(clusterState).asInstanceOf[JClusterState.CoupledOrDecoupled])
  }

  "ActiveShutDown" in {
    val clusterState = ClusterState.ActiveShutDown(setting)
    JClusterStateTester.testCoupledActiveShutDown(JClusterState(clusterState).asInstanceOf[JClusterState.ActiveShutDown])
    JClusterStateTester.testCoupledOrDecoupled(JClusterState(clusterState).asInstanceOf[JClusterState.CoupledOrDecoupled])
  }

  "PassiveLost" in {
    val clusterState = ClusterState.PassiveLost(setting)
    JClusterStateTester.testPassiveLost(JClusterState(clusterState).asInstanceOf[JClusterState.PassiveLost])
    JClusterStateTester.testCoupledOrDecoupled(JClusterState(clusterState).asInstanceOf[JClusterState.CoupledOrDecoupled])
  }

  "SwitchedOver" in {
    val clusterState = ClusterState.SwitchedOver(setting)
    JClusterStateTester.testSwitchedOver(JClusterState(clusterState).asInstanceOf[JClusterState.SwitchedOver])
    JClusterStateTester.testCoupledOrDecoupled(JClusterState(clusterState).asInstanceOf[JClusterState.CoupledOrDecoupled])
  }

  "FailedOver" in {
    val clusterState = ClusterState.FailedOver(setting, JournalPosition(0L, 0L))
    JClusterStateTester.testFailedOver(JClusterState(clusterState).asInstanceOf[JClusterState.FailedOver])
    JClusterStateTester.testCoupledOrDecoupled(JClusterState(clusterState).asInstanceOf[JClusterState.CoupledOrDecoupled])
  }
}
