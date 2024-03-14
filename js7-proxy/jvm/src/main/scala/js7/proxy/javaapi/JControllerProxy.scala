package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import io.vavr.control.Either as VEither
import java.util.Objects.requireNonNull
import java.util.concurrent.CompletableFuture
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.monixlike.MonixLikeExtensions.headL
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.data.controller.ControllerCommand.AddOrdersResponse
import js7.data.event.{Event, EventId, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.OrderTerminated
import js7.data_for_java.common.JavaUtils.Void
import js7.data_for_java.controller.JControllerState
import js7.data_for_java.order.JFreshOrder
import js7.data_for_java.reactor.ReactorConverters.*
import js7.data_for_java.vavr.VavrConverters.*
import js7.proxy.ControllerProxy
import js7.proxy.data.event.EventAndState
import js7.proxy.javaapi.data.controller.JEventAndControllerState
import js7.proxy.javaapi.eventbus.JControllerEventBus
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
  (using ioRuntime: IORuntime):

  private val prefetch = api.config.getInt("js7.web.server.prefetch")

  /** Listen to the already running event stream. */
  @Nonnull
  def flux(): Flux[JEventAndControllerState[Event]] =
    asScala.stream()
      .map(JEventAndControllerState.apply)
      .asFlux

  @Nonnull
  def stop(): CompletableFuture[Void] =
    asScala.stop
      .map(_ => Void)
      .unsafeToCompletableFuture()

  @Nonnull
  def currentState: JControllerState =
    JControllerState(asScala.currentState)

  /** Like JControllerApi addOrders, but waits until the Proxy mirrors the added orders. */
  @Nonnull
  def addOrders(@Nonnull orders: Flux[JFreshOrder]): CompletableFuture[VEither[Problem, AddOrdersResponse]] =
    asScala
      .addOrders:
        orders.asFs2Stream(bufferSize = prefetch).map(_.asScala)
      .map(_.toVavr)
      .unsafeToCompletableFuture()

  /**
    * Synchronize this mirror with an EventId.
    * The Future completes when this mirror has reached the requested EventId.
    */
  @Nonnull
  def sync(eventId: EventId): CompletableFuture[Void] =
    asScala.sync(eventId)
      .map(_ => Void)
      .unsafeToCompletableFuture()

  @Nonnull
  def when(@Nonnull predicate: JEventAndControllerState[Event] => Boolean): CompletableFuture[JEventAndControllerState[Event]] =
    requireNonNull(predicate)
    asScala.when(es => predicate(JEventAndControllerState(es)))
      .map(JEventAndControllerState.apply)
      .unsafeToCompletableFuture()

  @Nonnull
  private def runOrderForTest(@Nonnull order: JFreshOrder): CompletableFuture[Stamped[KeyedEvent[OrderTerminated]]] =
    requireNonNull(order)
    val whenOrderTerminated = asScala
      .subscribe().use:
        _.collect:
          case EventAndState(stamped @ Stamped(_, _, KeyedEvent(orderId, _: OrderTerminated)), _, _)
            if orderId == order.id =>
            stamped.asInstanceOf[Stamped[KeyedEvent[OrderTerminated]]]
        .headL
      .unsafeToCompletableFuture()

    val isAdded = api.asScala.addOrder(order.asScala)
      .logWhenItTakesLonger
      .await(99.s)
      .orThrow
    if !isAdded then throw new IllegalStateException(s"Order has already been added: ${order.id}")

    whenOrderTerminated
