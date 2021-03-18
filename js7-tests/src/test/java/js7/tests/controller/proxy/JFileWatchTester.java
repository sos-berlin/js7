package js7.tests.controller.proxy;

import java.io.FileOutputStream;
import java.nio.file.Path;
import java.util.concurrent.Future;
import js7.data.agent.AgentId;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.orderwatch.OrderWatchId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.orderwatch.JFileWatch;
import js7.proxy.javaapi.JControllerApi;
import reactor.core.publisher.Flux;
import static java.nio.file.Files.createTempDirectory;
import static java.nio.file.Files.delete;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.item.JUpdateItemOperation.addOrChangeSimple;
import static js7.data_for_java.vavr.VavrUtils.await;

final class JFileWatchTester
{
    private JFileWatchTester() {}

    static void test(JControllerApi api) throws Exception {
        Path directory = createTempDirectory("JFileWatchTester-");
        try {
            await(api.updateItems(Flux.just(addOrChangeSimple(
                JFileWatch.of(OrderWatchId.of("FILE-WATCH"),
                    WorkflowPath.of("WORKFLOW"), AgentId.of("AGENT"), directory)))));

            Path file = directory.resolve("TEST-FILE");
            OrderId orderId = OrderId.of("file:FILE-WATCH:TEST-FILE");

            Future<?> whenOrderRemoved = api.when(es ->
                es.stampedEvent().value().key().equals(orderId) &&
                es.stampedEvent().value().event() instanceof OrderEvent.OrderRemoved$);

            // Place the file
            new FileOutputStream(file.toFile()).close();

            whenOrderRemoved.get(99, SECONDS);
        } finally {
            delete(directory);
        }
    }
}
