package js7.tests.controller.proxy;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import js7.base.problem.Problem;
import js7.proxy.ProxyEvent;
import js7.proxy.javaapi.eventbus.JStandardEventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static java.util.Arrays.asList;

/** Holds the current coupling state updated by ClusterEvents.
 * @author Joacim Zschimmer
 */
public final class CouplingState implements AutoCloseable
{
    private static final Logger logger = LoggerFactory.getLogger(CouplingState.class);
    final CompletableFuture<Void> coupled = new CompletableFuture<>();
    final CompletableFuture<Void> decoupled = new CompletableFuture<>();
    final CompletableFuture<Problem> firstProblem = new CompletableFuture<>();
    Optional<Problem> lastProblem = Optional.empty();
    private final List<AutoCloseable> subscriptions = new LinkedList<>();

    public CouplingState(JStandardEventBus<ProxyEvent> eventBus) {
        subscriptions.add(eventBus.subscribe(asList(ProxyEvent.ProxyCoupled.class), this::onProxyCoupled));
        subscriptions.add(eventBus.subscribe(asList(ProxyEvent.ProxyDecoupled$.class), this::onProxyDecoupled));
        subscriptions.add(eventBus.subscribe(asList(ProxyEvent.ProxyCouplingError.class), this::onProxyCouplingError));
    }

    public void close() throws Exception {
        for (AutoCloseable o: subscriptions) o.close();
    }

    private void onProxyCoupled(ProxyEvent.ProxyCoupled proxyCoupled) {
        logger.info(proxyCoupled.toString());
        coupled.complete(null);
    }

    private void onProxyDecoupled(ProxyEvent.ProxyDecoupled$ proxyDecoupled) {
        logger.info(proxyDecoupled.toString());
        decoupled.complete(null);
    }

    private void onProxyCouplingError(ProxyEvent.ProxyCouplingError proxyCouplingError) {
        logger.info(proxyCouplingError.toString());
        firstProblem.complete(proxyCouplingError.problem());
        lastProblem = Optional.of(proxyCouplingError.problem());
    }
}
