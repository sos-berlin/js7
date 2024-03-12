package js7.tests.controller.proxy;

import java.io.FileOutputStream;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.Future;
import js7.data.agent.AgentPath;
import js7.data.order.OrderEvent.OrderDeleted$;
import js7.data.order.OrderId;
import js7.data.orderwatch.OrderWatchPath;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.controller.JControllerState;
import js7.data_for_java.orderwatch.JFileWatch;
import js7.data_for_java.value.JExpression;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Flux;
import static java.nio.file.Files.createTempDirectory;
import static java.nio.file.Files.delete;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.toList;
import static js7.data_for_java.item.JUpdateItemOperation.addOrChangeSimple;
import static js7.data_for_java.vavr.VavrUtils.await;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

final class JFileWatchTester
{
    private static final Logger logger = LoggerFactory.getLogger(JFileWatchTester.class);
    private static final OrderWatchPath fileWatchPath = OrderWatchPath.of("FILE-WATCH");
    private static final WorkflowPath workflowPath = WorkflowPath.of("WORKFLOW");

    private JFileWatchTester() {}

    static void testFileOrder(JControllerApi api) throws Exception {
        Path directory = createTempDirectory("JFileWatchTester-");
        try {
            await(
                api.updateItems(Flux.just(addOrChangeSimple(
                    getOrThrow(JFileWatch.checked(
                        fileWatchPath,
                        workflowPath,
                        AgentPath.of("AGENT"),
                        JExpression.fromString(directory.toString()),
                        Optional.of("file-(.+)\\.txt"),
                        Optional.of("'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \"#F$js7EpochSecond-$orderWatchPath:$1\""),
                        Duration.ofSeconds(0)))))));

            Path file = directory.resolve("file-TEST.txt");

            Future<?> whenOrderDeleted = api.when(es -> {
                // Fun with Java 17
                if (!(es.stampedEvent().value().key() instanceof OrderId))
                    return false;
                else {
                    String s = ((OrderId)es.stampedEvent().value().key()).string();
                    return
                        s.startsWith("#20") &&
                        s.contains("#F") &&
                        s.endsWith("-FILE-WATCH:TEST") &&
                        es.stampedEvent().value().event() instanceof OrderDeleted$;
                }
            });

            // Place the file
            new FileOutputStream(file.toFile()).close();

            whenOrderDeleted.get(99, SECONDS);

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

    static void testFileWatchApi(JControllerApi api) throws Exception {
        JControllerProxy proxy = api.startProxy().get(99, SECONDS);
        try {
            List<JFileWatch> fileWatches = workflowToOrderWatchPaths(proxy.currentState(), workflowPath);
            JFileWatch expected = proxy.currentState().pathToFileWatch().get(fileWatchPath);
            assertThat(fileWatches, equalTo(asList(expected)));
        } finally {
            proxy.stop().get(99, SECONDS);
        }
    }

    private static List<JFileWatch> workflowToOrderWatchPaths(JControllerState controllerState, WorkflowPath workflowPath) {
        return controllerState
            .pathToFileWatch()
            .values()
            .stream()
            .filter(o -> o.workflowPath().equals(workflowPath)).collect(toList());
    }
}
