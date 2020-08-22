package js7.proxy.javaapi.data.cluster

import js7.base.web.Uri
import js7.data.cluster.ClusterState
import js7.data.event.JournalPosition
import js7.data.node.NodeId
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JClusterStateTest extends AnyFreeSpec
{
  private val idToMap = Map(
    NodeId("Primary") -> Uri("http://primary"),
    NodeId("Backup") -> Uri("http://backup"))

  "Empty" in {
    JClusterStateTester.testEmpty(JClusterState(ClusterState.Empty).asInstanceOf[JClusterState.Empty.type])
  }

  "NodesAppointed" in {
    val clusterState = ClusterState.NodesAppointed(idToMap, NodeId("Primary"))
    JClusterStateTester.testNodesAppointed(JClusterState(clusterState).asInstanceOf[JClusterState.NodesAppointed])
  }

  "HasNodes" in {
    val clusterState = ClusterState.Coupled(idToMap, NodeId("Primary"))
    JClusterStateTester.testHasNodes(JClusterState(clusterState).asInstanceOf[JClusterState.HasNodes])
  }

  "Coupled" in {
    val clusterState = ClusterState.Coupled(idToMap, NodeId("Primary"))
    JClusterStateTester.testCoupled(JClusterState(clusterState).asInstanceOf[JClusterState.Coupled])
  }

  "PreparedToBeCoupled" in {
    val clusterState = ClusterState.PreparedToBeCoupled(idToMap, NodeId("Primary"))
    JClusterStateTester.testPreparedToBeCoupled(JClusterState(clusterState).asInstanceOf[JClusterState.PreparedToBeCoupled])
  }

  "Decoupled" in {
    val clusterState = ClusterState.PassiveLost(idToMap, NodeId("Primary"))
    JClusterStateTester.testDecoupled(JClusterState(clusterState).asInstanceOf[JClusterState.Decoupled])
  }

  "CoupledActiveShutDown" in {
    val clusterState = ClusterState.CoupledActiveShutDown(idToMap, NodeId("Primary"))
    JClusterStateTester.testCoupledActiveShutDown(JClusterState(clusterState).asInstanceOf[JClusterState.CoupledActiveShutDown])
  }

  "PassiveLost" in {
    val clusterState = ClusterState.PassiveLost(idToMap, NodeId("Primary"))
    JClusterStateTester.testPassiveLost(JClusterState(clusterState).asInstanceOf[JClusterState.PassiveLost])
  }

  "SwitchedOver" in {
    val clusterState = ClusterState.SwitchedOver(idToMap, NodeId("Primary"))
    JClusterStateTester.testSwitchedOver(JClusterState(clusterState).asInstanceOf[JClusterState.SwitchedOver])
  }

  "FailedOver" in {
    val clusterState = ClusterState.FailedOver(idToMap, NodeId("Primary"), JournalPosition(0L, 0L))
    JClusterStateTester.testFailedOver(JClusterState(clusterState).asInstanceOf[JClusterState.FailedOver])
  }
}
