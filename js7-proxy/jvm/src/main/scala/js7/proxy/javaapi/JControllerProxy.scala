package js7.proxy.javaapi

import io.vavr.control.{Either => VEither}
import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.base.generic.Completed
import js7.base.problem.Problem
import js7.controller.data.ControllerCommand
import js7.data.event.Event
import js7.proxy.javaapi.data.{JControllerCommand, JControllerState, JFreshOrder}
import js7.proxy.javaapi.utils.JavaUtils.Void
import js7.proxy.javaapi.utils.VavrConversions._
import js7.proxy.{ControllerProxy, ProxyEvent}
import monix.execution.FutureUtils.Java8Extensions
import monix.reactive.Observable
import reactor.core.publisher.Flux

/** Java adapter for `JournaledProxy[JControllerState]`. */
@javaApi
final class JControllerProxy private[proxy](
  private[js7] val controllerProxy: ControllerProxy,
  val proxyEventBus: JStandardEventBus[ProxyEvent],
  val controllerEventBus: JControllerEventBus,
  context: JProxyContext)
{
  import context.scheduler

  def startObserving: CompletableFuture[Unit] =
    controllerProxy.startObserving
      .runToFuture
      .asJava

  def flux: Flux[JEventAndControllerState[Event]] =
    Flux.from(
      controllerProxy.observe
        .map(JEventAndControllerState.fromScala)
        .toReactivePublisher)

  def stop: CompletableFuture[java.lang.Void] =
    controllerProxy.stop
      .map(_ => null: java.lang.Void)
      .runToFuture
      .asJava

  def currentState: JControllerState =
    JControllerState(controllerProxy.currentState._2)

  /** @return true iff added, false iff not added because of duplicate OrderId. */
  def addOrder(order: JFreshOrder): CompletableFuture[VEither[Problem, java.lang.Boolean]] =
    controllerProxy.addOrder(order.underlying)
      .map(_.map(o => java.lang.Boolean.valueOf(o)).toVavr)
      .runToFuture
      .asJava

  def addOrders(orders: java.lang.Iterable[JFreshOrder]): CompletableFuture[VEither[Problem, java.lang.Void]] =
    addOrders(Flux.fromIterable(orders))

  def addOrders(orders: Flux[JFreshOrder]): CompletableFuture[VEither[Problem, java.lang.Void]] =
    controllerProxy.addOrders(Observable.fromReactivePublisher(orders).map(_.underlying))
      .map(_.map((_: Completed) => Void).toVavr)
      .runToFuture
      .asJava

  def executeCommand(command: JControllerCommand): CompletableFuture[VEither[Problem, ControllerCommand.Response]] =
    controllerProxy.executeCommand(command.underlying)
      .map(_.map(o => (o: ControllerCommand.Response)).toVavr)
      .runToFuture
      .asJava

  def executeCommandJson(command: String): CompletableFuture[VEither[Problem, String]] =
    httpPostJson("/controller/api/command", command)

  def httpPostJson(uriTail: String, jsonString: String): CompletableFuture[VEither[Problem, String]] =
    controllerProxy.httpPostJson(uriTail, jsonString)
      .map(_.toVavr)
      .runToFuture
      .asJava

  /** HTTP GET
    * @param uriTail path and query of the URI
    * @return `Either.Left(Problem)` or `Either.Right(json: String)`
    */
  def httpGetJson(uriTail: String): CompletableFuture[VEither[Problem, String]] =
    controllerProxy.httpGetJson(uriTail)
      .map(_.toVavr)
      .runToFuture
      .asJava
}
