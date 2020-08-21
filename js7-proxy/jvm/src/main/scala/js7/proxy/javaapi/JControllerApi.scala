package js7.proxy.javaapi

import cats.effect.Resource
import io.vavr.control.{Either => VEither}
import java.util.OptionalLong
import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.controller.client.HttpControllerApi
import js7.controller.data.ControllerCommand
import js7.data.event.Event
import js7.data.item.VersionId
import js7.proxy.configuration.ProxyConf
import js7.proxy.javaapi.data.{JControllerCommand, JFreshOrder, JUpdateRepoOperation}
import js7.proxy.javaapi.eventbus.{JControllerEventBus, JStandardEventBus}
import js7.proxy.javaapi.utils.ReactorConverters._
import js7.proxy.javaapi.utils.VavrConverters._
import js7.proxy.{ControllerApi, ControllerProxy, ProxyEvent}
import monix.eval.Task
import monix.execution.FutureUtils.Java8Extensions
import monix.execution.Scheduler
import reactor.core.publisher.Flux
import scala.jdk.OptionConverters._

@javaApi
final class JControllerApi private[js7](
  apiResources: Seq[Resource[Task, HttpControllerApi]],
  proxyConf: ProxyConf)
  (implicit scheduler: Scheduler)
{
  private[js7] val underlying = new ControllerApi(apiResources, proxyConf)

  /** Fetch event stream from Controller. */
  def flux(proxyEventBus: JStandardEventBus[ProxyEvent]): Flux[JEventAndControllerState[Event]] =
    flux(proxyEventBus, OptionalLong.empty())

  /** Fetch event stream from Controller. */
  def flux(proxyEventBus: JStandardEventBus[ProxyEvent], after: OptionalLong/*EventId*/): Flux[JEventAndControllerState[Event]] =
    underlying.observable(proxyEventBus.underlying, after.toScala)
      .map(JEventAndControllerState.apply)
      .asFlux

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

  /** Update the Repo, i.e. add, change or delete inventory items.
    *
    * Each `JUpdateRepoOperation` adds/replaces or deletes an item.
    *
    * '''To add or replace an item:'''
    * {{{
    * JUpdateRepoOperations.addOrReplace(
    *   SignedString.of(
    *     jsonString,
    *     "PGP"/*for example*/,
    *     signatureString)
    * }}}
    * `SignedString.of` requires three arguments:
    *   - `jsonString` is the JSON-encoded `InventoryItem`, i.e. a Workflow or an AgentRefPath.
    *     The item must include its id with `path` and `versionId`.
    *     The `versionId` must be the same as the first argument for `updateRepo`.
    *   - "PGP" or any supported signature type.
    *   - `signatureString` is the the signature of the UTF-8 encoded `jsonString`.
    *     {{{
    * signatureString = sign(jsonString.getBytes(UTF_8))
    *     }}}
    *
    * '''To delete an item:'''
    *
    * {{{
    * JUpdateRepoOperations.addOrReplace(TypedPath)
    * }}}
    *
    * `TypedPath` may be a [[js7.data.workflow.WorkflowPath]] or a [[js7.data.agent.AgentRefPath]]
    * (both have a Java-compatible static factory method `of`).
    *
    * @param versionId `VersionId` of this new version
    * @param operations Stream of JUpdateRepoOperations
    *
    */
  def updateRepo(versionId: VersionId, operations: Flux[JUpdateRepoOperation]): CompletableFuture[VEither[Problem, Void]] =
    underlying.updateRepo(versionId, operations.asObservable.map(_.underlying))
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  /** @return true iff added, false iff not added because of duplicate OrderId. */
  def addOrder(order: JFreshOrder): CompletableFuture[VEither[Problem, java.lang.Boolean]] =
    underlying.addOrder(order.underlying)
      .map(_.map(o => java.lang.Boolean.valueOf(o)).toVavr)
      .runToFuture
      .asJava

  /** Add `Order`s provided by a Reactor stream.
    *
    * The Controller stores the whole stream as a single commit.
    * The Controller adds the orders at the end of the stream, doing a single big commit.
    * At any error, all orders are rejected.
    *
    * An `Iterable&lt;Order>` can be added using the call
    *
    * {{{api.addOrders(Flux.fromIterable(orders))}}}
    * */
  def addOrders(orders: Flux[JFreshOrder]): CompletableFuture[VEither[Problem, Void]] =
    underlying.addOrders(orders.asObservable.map(_.underlying))
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  def executeCommand(command: JControllerCommand): CompletableFuture[VEither[Problem, ControllerCommand.Response]] =
    underlying.executeCommand(command.underlying)
      .map(_.map(o => (o: ControllerCommand.Response)).toVavr)
      .runToFuture
      .asJava

  def executeCommandJson(command: String): CompletableFuture[VEither[Problem, String]] =
    httpPostJson("/controller/api/command", command)

  def httpPostJson(uriTail: String, jsonString: String): CompletableFuture[VEither[Problem, String]] =
    underlying.httpPostJson(uriTail, jsonString)
      .map(_.toVavr)
      .runToFuture
      .asJava

  /** HTTP GET
    * @param uriTail path and query of the URI
    * @return `Either.Left(Problem)` or `Either.Right(json: String)`
    */
  def httpGetJson(uriTail: String): CompletableFuture[VEither[Problem, String]] =
    underlying.httpGetJson(uriTail)
      .map(_.toVavr)
      .runToFuture
      .asJava
}
