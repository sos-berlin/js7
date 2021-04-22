package js7.tests.controller.proxy;

import java.io.FileOutputStream;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Optional;
import java.util.concurrent.Future;
import js7.data.agent.AgentPath;
import js7.data.order.OrderEvent.OrderRemoved$;
import js7.data.order.OrderId;
import js7.data.orderwatch.OrderWatchPath;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.orderwatch.JFileWatch;
import js7.proxy.javaapi.JControllerApi;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Flux;
import static java.nio.file.Files.createTempDirectory;
import static java.nio.file.Files.delete;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.item.JUpdateItemOperation.addOrChangeSimple;
import static js7.data_for_java.vavr.VavrUtils.await;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;

final class JFileWatchTester
{
    private static final Logger logger = LoggerFactory.getLogger(JFileWatchTester.class);

    private JFileWatchTester() {}

    static void test(JControllerApi api) throws Exception {
        Path directory = createTempDirectory("JFileWatchTester-");
        try {
            await(
                api.updateItems(Flux.just(addOrChangeSimple(
                    getOrThrow(JFileWatch.checked(
                        OrderWatchPath.of("FILE-WATCH"),
                        WorkflowPath.of("WORKFLOW"),
                        AgentPath.of("AGENT"),
                        directory,
                        Optional.of("file-(.+)\\.txt"),
                        Optional.of("'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \"#F$epochSecond-$orderWatchPath:$1\""),
                        Duration.ofSeconds(0)))))));

            Path file = directory.resolve("file-TEST.txt");

            Future<?> whenOrderRemoved = api.when(es -> {
                // Fun with Java
                if (!(es.stampedEvent().value().key() instanceof OrderId))
                    return false;
                else {
                    String s = ((OrderId)es.stampedEvent().value().key()).string();
                    return
                        s.startsWith("#20") &&
                        s.contains("#F") &&
                        s.endsWith("-FILE-WATCH:TEST") &&
                        es.stampedEvent().value().event() instanceof OrderRemoved$;
                }
            });

            // Place the file
            new FileOutputStream(file.toFile()).close();

            whenOrderRemoved.get(99, SECONDS);

            delete(directory);
        } catch (Throwable t) {
            try {
                delete(directory);
            } catch (Throwable t2) {
                logger.error(t2.toString(), t);
            }
            throw t;
        }
    }
}
