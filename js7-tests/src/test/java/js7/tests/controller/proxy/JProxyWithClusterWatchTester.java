package js7.tests.controller.proxy;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import js7.data.cluster.ClusterWatchId;
import js7.data_for_java.auth.JAdmission;
import js7.data_for_java.auth.JHttpsConfig;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JProxyContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

            controllerApi.startClusterWatch(ClusterWatchId.of("JOC-A")).get();
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
