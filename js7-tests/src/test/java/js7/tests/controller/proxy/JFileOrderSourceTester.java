package js7.tests.controller.proxy;

import java.io.FileOutputStream;
import java.nio.file.Path;
import java.util.concurrent.Future;
import js7.data.agent.AgentId;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.ordersource.OrderSourceId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.ordersource.JFileOrderSource;
import js7.proxy.javaapi.JControllerApi;
import reactor.core.publisher.Flux;
import static java.nio.file.Files.createTempDirectory;
import static java.nio.file.Files.delete;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.item.JUpdateItemOperation.addOrChangeSimple;
import static js7.data_for_java.vavr.VavrUtils.await;

class JFileOrderSourceTester
{
    private JFileOrderSourceTester() {}

    static void test(JControllerApi api) throws Exception {
        Path directory = createTempDirectory("JFileOrderSourceTester-");
        try {
            await(api.updateItems(Flux.just(addOrChangeSimple(
                JFileOrderSource.of(OrderSourceId.of("ORDER-SOURCE"),
                    WorkflowPath.of("WORKFLOW"), AgentId.of("AGENT"), directory)))));

            Path file = directory.resolve("TEST-FILE");
            OrderId orderId = OrderId.of("FileOrderSource:ORDER-SOURCE:TEST-FILE");

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
