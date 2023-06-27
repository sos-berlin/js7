package js7.tests.controller.proxy;

import io.vavr.control.Either;
import js7.base.eventbus.StandardEventBus;
import js7.base.problem.Problem;
import js7.cluster.watch.ClusterWatchService;
import js7.cluster.watch.api.ClusterWatchProblems.ClusterNodeLossNotConfirmedProblem;
import js7.data.cluster.ClusterEvent;
import js7.data.cluster.ClusterWatchId;
import js7.data.node.NodeId;
import js7.data_for_java.auth.JAdmission;
import js7.data_for_java.auth.JHttpsConfig;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.eventbus.JStandardEventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static java.util.Arrays.asList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

final class JProxyWithClusterWatchTester
{
    private static final Logger logger = LoggerFactory.getLogger(JProxyWithClusterWatchTester.class);
    private JProxyWithClusterWatchTester() {}

    static void test(List<JAdmission> admissions, JHttpsConfig httpsConfig)
        throws Exception
    {
        try (JProxyContext context = new JProxyContext()) {
            JControllerApi controllerApi = context
                .newControllerApi(admissions, httpsConfig);

            JStandardEventBus<ClusterNodeLossNotConfirmedProblem> eventBus =
                new JStandardEventBus<>(new StandardEventBus<>(ClusterNodeLossNotConfirmedProblem.class));
            eventBus.subscribe(
                asList(ClusterNodeLossNotConfirmedProblem.class),
                problem -> logger.info("Event received: " + problem));
            ClusterWatchService clusterWatchService =
                controllerApi.startClusterWatch(
                    ClusterWatchId.of("JOC-A"),
                    o -> eventBus.publish(o)).get();

            // ClusterWatchService provides some methods
            assertThat(clusterWatchService.clusterWatchId(), equalTo(ClusterWatchId.of("JOC-A")));
            clusterWatchService.clusterWatchRunId();
            clusterWatchService.clusterState();

            NodeId primaryId = NodeId.primary();
            NodeId backupId = NodeId.backup();
            if (clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId).isDefined()) {
                ClusterEvent.ClusterNodeLostEvent clusterNodeLostEvent =
                    clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId).get();
                assert clusterNodeLostEvent.lostNodeId().equals(primaryId);

                // In case of broken connection between the nodes, both primaryId and backupId
                // may require manual ClusterNodeLostEvent.
                // The user must decide which node is considered to be lost.
                // Before this, they must terminate the lost node.
                assert clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId).isEmpty();

                // Don't do this automatically! The user must be sure that the node is down.
                // Otherwise, both cluster nodes may get active, with destroying consequences.

                Either<Problem,?> checked = controllerApi.manuallyConfirmNodeLoss(primaryId, "CONFIRMER").get();
            }

            // Stop is effective only after startClusterWatch has completed!
            controllerApi.stopClusterWatch().get();

            // Run again
            CompletableFuture<Void> clusterWatchStopped =
                controllerApi.runClusterWatch(ClusterWatchId.of("JOC-A"))
                    .thenRun(() -> logger.info("clusterWatchStopped COMPLETED"));

            Thread.sleep(1000);

            if (clusterWatchStopped.isDone()) {
                clusterWatchStopped.get()/*throws*/;
                throw new IllegalStateException("ClusterWatch has stopped unexpectedly");
            }

            // controllerApi.stop stpos ClusterWatch, too
            controllerApi.stop().get();
            clusterWatchStopped.get()/*throws*/;
        }
    }
}
