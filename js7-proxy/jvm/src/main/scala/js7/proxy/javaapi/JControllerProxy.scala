package js7.proxy.javaapi

import io.vavr.control.{Either => VEither}
import java.util.Objects.requireNonNull
import java.util.concurrent.CompletableFuture
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand.AddOrdersResponse
import js7.data.event.{Event, EventId, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.OrderTerminated
import js7.proxy.ControllerProxy
import js7.proxy.data.event.EventAndState
import js7.proxy.javaapi.data.common.JavaUtils.Void
import js7.proxy.javaapi.data.common.ReactorConverters._
import js7.proxy.javaapi.data.common.VavrConverters._
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
  asScala: ControllerProxy,
  val api: JControllerApi,
  val controllerEventBus: JControllerEventBus)
  (implicit scheduler: Scheduler)
{
  /** Listen to the already running event stream. */
  @Nonnull
  def flux(): Flux[JEventAndControllerState[Event]] =
    asScala.observable
      .map(JEventAndControllerState.apply)
      .asFlux

  @Nonnull
  def stop(): CompletableFuture[Void] =
    asScala.stop
      .map(_ => Void)
      .runToFuture
      .asJava

  @Nonnull
  def currentState: JControllerState =
    JControllerState(asScala.currentState)

  /** Like JControllerApi addOrders, but waits until the Proxy mirrors the added orders. */
  @Nonnull
  def addOrders(@Nonnull orders: Flux[JFreshOrder]): CompletableFuture[VEither[Problem, AddOrdersResponse]] =
    asScala.addOrders(orders.asObservable.map(_.asScala))
      .map(_.toVavr)
      .runToFuture
      .asJava

  /**
    * Synchronize this mirror with an EventId.
    * The Future completes when this mirror has reached the requested EventId.
    */
  @Nonnull
  def sync(eventId: EventId): CompletableFuture[Void] =
    asScala.sync(eventId)
      .map(_ => Void)
      .runToFuture
      .asJava

  @Nonnull
  def when(@Nonnull predicate: JEventAndControllerState[Event] => Boolean): CompletableFuture[JEventAndControllerState[Event]] = {
    requireNonNull(predicate)
    asScala.when(es => predicate(JEventAndControllerState(es)))
      .map(JEventAndControllerState.apply)
      .runToFuture
      .asJava
  }

  @Nonnull
  private def runOrderForTest(@Nonnull order: JFreshOrder): CompletableFuture[Stamped[KeyedEvent[OrderTerminated]]] = {
    requireNonNull(order)
    val whenOrderTerminated = asScala.observable
      .collect {
        case EventAndState(stamped @ Stamped(_, _, KeyedEvent(orderId, _: OrderTerminated)), _, _)
          if orderId == order.id =>
          stamped.asInstanceOf[Stamped[KeyedEvent[OrderTerminated]]]
      }
      .headL
      .runToFuture
    val isAdded = api.asScala.addOrder(order.asScala)
      .await(99.s)
      .orThrow
    if (!isAdded) throw new IllegalStateException(s"Order has already been added: ${order.id}")
    whenOrderTerminated.asJava
  }
}
