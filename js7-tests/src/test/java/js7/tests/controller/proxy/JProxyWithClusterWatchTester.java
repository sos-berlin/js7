package js7.tests.controller.proxy;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import js7.base.problem.Problem;
import js7.cluster.watch.ClusterWatchService;
import js7.data.cluster.ClusterEvent;
import js7.data.cluster.ClusterWatchId;
import js7.data.node.NodeId;
import js7.data_for_java.auth.JAdmission;
import js7.data_for_java.auth.JHttpsConfig;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JProxyContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.compat.java8.OptionConverters;
import scala.util.Either;
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

            ClusterWatchService clusterWatchService =
                controllerApi.startClusterWatch(ClusterWatchId.of("JOC-A")).get();

            // ClusterWatchService provides some methods
            assertThat(clusterWatchService.clusterWatchId(), equalTo(ClusterWatchId.of("JOC-A")));
            clusterWatchService.clusterWatchRunId();
            clusterWatchService.clusterState();

            if (clusterWatchService.clusterFailedOverRequested().isDefined()) {
                ClusterEvent.ClusterFailedOver clusterFailedOver =
                    clusterWatchService.clusterFailedOverRequested().get();
                NodeId lostNodeId = clusterFailedOver.lostNodeId();
                // Don't do this automatically! The user must be sure that the node is down.
                // Otherwise, both cluster nodes may get active, with destroying consequences.
                Either<Problem,?> checked = clusterWatchService.confirmNodeLoss(lostNodeId);
                Optional<Problem> maybeProblem = OptionConverters.toJava(checked.left().toOption());
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
