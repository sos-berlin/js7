package js7.tests.controller.proxy;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import js7.data.cluster.ClusterWatchId;
import js7.data_for_java.auth.JAdmission;
import js7.data_for_java.auth.JHttpsConfig;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JProxyContext;

final class JProxyWithClusterWatchTester
{
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

            // Start again
            CompletableFuture<Void> clusterWatchStopped =
                controllerApi.startClusterWatch(ClusterWatchId.of("JOC-A")).get();

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
