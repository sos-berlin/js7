package js7.proxy.javaapi

import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.common.scalautil.MonixUtils.syntax._
import js7.data.event.{Event, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.OrderTerminated
import js7.proxy.ControllerProxy
import js7.proxy.data.event.EventAndState
import js7.proxy.javaapi.data.common.JavaUtils.Void
import js7.proxy.javaapi.data.common.ReactorConverters._
import js7.proxy.javaapi.data.controller.{JControllerState, JEventAndControllerState}
import js7.proxy.javaapi.data.order.JFreshOrder
import js7.proxy.javaapi.eventbus.JControllerEventBus
import monix.execution.FutureUtils.Java8Extensions
import monix.execution.Scheduler
import reactor.core.publisher.Flux

/** Observes the Controller's event stream and provides the current JControllerState.
  * After use, stop with `stop()`.
  *
  * Java adapter for `JournaledProxy[JControllerState]`. */
@javaApi
final class JControllerProxy private[proxy](
  underlying: ControllerProxy,
  val api: JControllerApi,
  val controllerEventBus: JControllerEventBus)
  (implicit scheduler: Scheduler)
{
  /** Listen to the already running event stream. */
  def flux(): Flux[JEventAndControllerState[Event]] =
    underlying.observable
      .map(JEventAndControllerState.apply)
      .asFlux

  def stop(): CompletableFuture[Void] =
    underlying.stop
      .map(_ => Void)
      .runToFuture
      .asJava

  def currentState: JControllerState =
    JControllerState(underlying.currentState)

  private def runOrderForTest(order: JFreshOrder): CompletableFuture[Stamped[KeyedEvent[OrderTerminated]]] = {
    val whenOrderTerminated = underlying.observable
      .collect {
        case EventAndState(stamped @ Stamped(_, _, KeyedEvent(orderId, _: OrderTerminated)), _, _)
          if orderId == order.id =>
          stamped.asInstanceOf[Stamped[KeyedEvent[OrderTerminated]]]
      }
      .headL
      .runToFuture
    val isAdded = api.underlying.addOrder(order.asScala)
      .await(99.s)
      .orThrow
    if (!isAdded) throw new IllegalStateException(s"Order has already been added: ${order.id}")
    whenOrderTerminated.asJava
  }
}
