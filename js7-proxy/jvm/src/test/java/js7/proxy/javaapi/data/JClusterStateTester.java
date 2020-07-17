package js7.proxy.javaapi.data;

import java.util.HashMap;
import java.util.Map;
import js7.base.web.Uri;
import js7.data.node.NodeId;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * @author Joacim Zschimmer
 */
public class JClusterStateTester
{
    private static final Map<NodeId, Uri> idToUri = new HashMap<NodeId, Uri>() {{
        put(NodeId.unchecked("Primary"), Uri.unchecked("http://primary"));
        put(NodeId.unchecked("Backup"), Uri.unchecked("http://backup"));
    }};
    private JClusterStateTester() {}

    static void testEmpty(JClusterState.Empty clusterState) {
        // No fields
    }

    static void testHasNodes(JClusterState.HasNodes clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }

    static void testCoupledOrDecoupled(JClusterState.CoupledOrDecoupled clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }

    static void testDecoupled(JClusterState.Decoupled clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }

    static void testNodesAppointed(JClusterState.NodesAppointed clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }

    static void testPreparedToBeCoupled(JClusterState.PreparedToBeCoupled clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }

    static void testCoupled(JClusterState.Coupled clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }

    static void testCoupledActiveShutDown(JClusterState.CoupledActiveShutDown clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }

    static void testPassiveLost(JClusterState.PassiveLost clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }

    static void testSwitchedOver(JClusterState.SwitchedOver clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }

    static void testFailedOver(JClusterState.FailedOver clusterState) {
        assertThat(clusterState.idToUri(), equalTo(idToUri));
    }
}
