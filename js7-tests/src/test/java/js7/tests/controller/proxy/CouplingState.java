package js7.tests.controller.proxy;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import js7.base.problem.Problem;
import js7.proxy.ProxyEvent;
import js7.proxy.javaapi.JStandardEventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static java.util.Arrays.asList;

/**
 * @author Joacim Zschimmer
 */
final class CouplingState
{
    private static final Logger logger = LoggerFactory.getLogger(CouplingState.class);
    final CompletableFuture<Void> coupled = new CompletableFuture<>();
    final CompletableFuture<Void> decoupled = new CompletableFuture<>();
    final CompletableFuture<Problem> firstProblem = new CompletableFuture<>();
    Optional<Problem> lastProblem = Optional.empty();

    void onProxyCoupled(ProxyEvent.ProxyCoupled proxyCoupled) {
        logger.info(proxyCoupled.toString());
        coupled.complete(null);
    }

    void onProxyDecoupled(ProxyEvent.ProxyDecoupled$ proxyDecoupled) {
        logger.info(proxyDecoupled.toString());
        decoupled.complete(null);
    }

    void onProxyCouplingError(ProxyEvent.ProxyCouplingError proxyCouplingError) {
        logger.info(proxyCouplingError.toString());
        firstProblem.complete(proxyCouplingError.problem());
        lastProblem = Optional.of(proxyCouplingError.problem());
    }

    void subscribe(JStandardEventBus<ProxyEvent> eventBus) {
        eventBus.subscribe(asList(ProxyEvent.ProxyCoupled.class), this::onProxyCoupled);
        eventBus.subscribe(asList(ProxyEvent.ProxyDecoupled$.class), this::onProxyDecoupled);
        eventBus.subscribe(asList(ProxyEvent.ProxyCouplingError.class), this::onProxyCouplingError);
    }
}
