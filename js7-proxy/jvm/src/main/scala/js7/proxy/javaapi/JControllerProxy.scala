package js7.proxy.javaapi

import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.data.event.Event
import js7.proxy.ControllerProxy
import js7.proxy.javaapi.data.JControllerState
import js7.proxy.javaapi.utils.JavaUtils.Void
import monix.execution.FutureUtils.Java8Extensions
import monix.execution.Scheduler
import reactor.core.publisher.Flux

/** Observes the Controller's event stream and provides the current JControllerState.
  * After use, stop with `stop()`.
  *
  * Java adapter for `JournaledProxy[JControllerState]`. */
@javaApi
final class JControllerProxy private[proxy](
  controllerProxy: ControllerProxy,
  val api: JControllerApi,
  val controllerEventBus: JControllerEventBus)
  (implicit scheduler: Scheduler)
{
  def flux(): Flux[JEventAndControllerState[Event]] =
    Flux.from(
      controllerProxy.observable
        .map(JEventAndControllerState .fromScala)
        .toReactivePublisher)

  def stop(): CompletableFuture[java.lang.Void] =
    controllerProxy.stop
      .map(_ => Void)
      .runToFuture
      .asJava

  def currentState: JControllerState =
    JControllerState(controllerProxy.currentState)
}
