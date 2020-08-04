package js7.proxy.javaapi

import cats.effect.Resource
import io.vavr.control.{Either => VEither}
import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.base.generic.Completed
import js7.base.problem.Problem
import js7.controller.client.HttpControllerApi
import js7.controller.data.ControllerCommand
import js7.data.item.VersionId
import js7.proxy.configuration.ProxyConf
import js7.proxy.javaapi.data.{JControllerCommand, JFreshOrder, JUpdateRepoOperation}
import js7.proxy.javaapi.eventbus.{JControllerEventBus, JStandardEventBus}
import js7.proxy.javaapi.utils.JavaUtils.Void
import js7.proxy.javaapi.utils.VavrConversions._
import js7.proxy.{ControllerApi, ControllerProxy, ProxyEvent}
import monix.eval.Task
import monix.execution.FutureUtils.Java8Extensions
import monix.execution.Scheduler
import monix.reactive.Observable
import reactor.core.publisher.Flux

@javaApi
final class JControllerApi private[proxy](
  apiResources: Seq[Resource[Task, HttpControllerApi]],
  private[js7] val api: ControllerApi,
  proxyConf: ProxyConf)
  (implicit scheduler: Scheduler)
{
  def startProxy(): CompletableFuture[JControllerProxy] =
    startProxy(new JStandardEventBus[ProxyEvent])

  def startProxy(proxyEventBus: JStandardEventBus[ProxyEvent]): CompletableFuture[JControllerProxy] =
    startProxy(proxyEventBus, new JControllerEventBus)

  /** Starts a `JControllerProxy`.
    * After use, stop it with `JControllerProxy.stop()`. */
  def startProxy(
    proxyEventBus: JStandardEventBus[ProxyEvent],
    controllerEventBus: JControllerEventBus)
  : CompletableFuture[JControllerProxy] = {
    ControllerProxy.start(
      apiResources,
      proxyEventBus.underlying,
      controllerEventBus.underlying,
      proxyConf
    ) .map(new JControllerProxy(_, this, controllerEventBus))
      .runToFuture
      .asJava
  }

  def updateRepo(versionId: VersionId, operations: Flux[JUpdateRepoOperation]): CompletableFuture[VEither[Problem, Void]] =
    api.updateRepo(versionId, Observable.fromReactivePublisher(operations).map(_.underlying))
      .map(_.map((_: Completed) => Void).toVavr)
      .runToFuture
      .asJava

  /** @return true iff added, false iff not added because of duplicate OrderId. */
  def addOrder(order: JFreshOrder): CompletableFuture[VEither[Problem, java.lang.Boolean]] =
    api.addOrder(order.underlying)
      .map(_.map(o => java.lang.Boolean.valueOf(o)).toVavr)
      .runToFuture
      .asJava

  def addOrders(orders: java.lang.Iterable[JFreshOrder]): CompletableFuture[VEither[Problem, java.lang.Void]] =
    addOrders(Flux.fromIterable(orders))

  def addOrders(orders: Flux[JFreshOrder]): CompletableFuture[VEither[Problem, java.lang.Void]] =
    api.addOrders(Observable.fromReactivePublisher(orders).map(_.underlying))
      .map(_.map((_: Completed) => Void).toVavr)
      .runToFuture
      .asJava

  def executeCommand(command: JControllerCommand): CompletableFuture[VEither[Problem, ControllerCommand.Response]] =
    api.executeCommand(command.underlying)
      .map(_.map(o => (o: ControllerCommand.Response)).toVavr)
      .runToFuture
      .asJava

  def executeCommandJson(command: String): CompletableFuture[VEither[Problem, String]] =
    httpPostJson("/controller/api/command", command)

  def httpPostJson(uriTail: String, jsonString: String): CompletableFuture[VEither[Problem, String]] =
    api.httpPostJson(uriTail, jsonString)
      .map(_.toVavr)
      .runToFuture
      .asJava

  /** HTTP GET
    * @param uriTail path and query of the URI
    * @return `Either.Left(Problem)` or `Either.Right(json: String)`
    */
  def httpGetJson(uriTail: String): CompletableFuture[VEither[Problem, String]] =
    api.httpGetJson(uriTail)
      .map(_.toVavr)
      .runToFuture
      .asJava
}
