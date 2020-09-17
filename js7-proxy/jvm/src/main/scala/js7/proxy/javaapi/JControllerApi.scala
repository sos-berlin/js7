package js7.proxy.javaapi

import cats.effect.Resource
import io.vavr.control.{Either => VEither}
import java.util.concurrent.CompletableFuture
import java.util.{Optional, OptionalLong}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.controller.client.HttpControllerApi
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.{CancelOrders, ReleaseEvents, RemoveOrdersWhenTerminated, ResumeOrders, SuspendOrders}
import js7.data.command.CancelMode
import js7.data.event.{Event, EventId}
import js7.data.item.VersionId
import js7.data.order.OrderId
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.ProxyEvent
import js7.proxy.javaapi.data.common.JavaUtils.Void
import js7.proxy.javaapi.data.common.ReactorConverters._
import js7.proxy.javaapi.data.common.VavrConverters._
import js7.proxy.javaapi.data.controller.{JControllerCommand, JEventAndControllerState}
import js7.proxy.javaapi.data.item.JUpdateRepoOperation
import js7.proxy.javaapi.data.order.JFreshOrder
import js7.proxy.javaapi.data.workflow.position.JPosition
import js7.proxy.javaapi.eventbus.{JControllerEventBus, JStandardEventBus}
import js7.proxy.{ControllerApi, ControllerProxy}
import monix.eval.Task
import monix.execution.FutureUtils.Java8Extensions
import monix.execution.Scheduler
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

@javaApi
final class JControllerApi private[javaapi](
  apiResources: Seq[Resource[Task, HttpControllerApi]],
  proxyConf: ProxyConf)
  (implicit scheduler: Scheduler)
{
  private[js7] val asScala = new ControllerApi(apiResources, proxyConf)

  /** Fetch event stream from Controller. */
  def eventFlux(proxyEventBus: JStandardEventBus[ProxyEvent]): Flux[JEventAndControllerState[Event]] =
    eventFlux(proxyEventBus, OptionalLong.empty())

  /** Fetch event stream from Controller. */
  def eventFlux(proxyEventBus: JStandardEventBus[ProxyEvent], after: OptionalLong/*EventId*/): Flux[JEventAndControllerState[Event]] =
    asScala.eventObservable(proxyEventBus.asScala, after.toScala)
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
  : CompletableFuture[JControllerProxy] =
    ControllerProxy.start(
      apiResources,
      proxyEventBus.asScala,
      controllerEventBus.asScala,
      proxyConf
    ) .map(new JControllerProxy(_, this, controllerEventBus))
      .runToFuture
      .asJava

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
    asScala.updateRepo(versionId, operations.asObservable.map(_.asScala))
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  /** @return true iff added, false iff not added because of duplicate OrderId. */
  def addOrder(order: JFreshOrder): CompletableFuture[VEither[Problem, java.lang.Boolean]] =
    asScala.addOrder(order.asScala)
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
    asScala.addOrders(orders.asObservable.map(_.asScala))
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  def cancelOrders(orderIds: java.lang.Iterable[OrderId]): CompletableFuture[VEither[Problem, Void]] =
    execute(CancelOrders(orderIds.asScala.toVector, CancelMode.FreshOrStarted()))

  def suspendOrders(orderIds: java.lang.Iterable[OrderId]): CompletableFuture[VEither[Problem, Void]] =
    execute(SuspendOrders(orderIds.asScala.toVector))

  def resumeOrders(orderId: java.lang.Iterable[OrderId]): CompletableFuture[VEither[Problem, Void]] =
    resumeOrders(orderId, Optional.empty)

  def resumeOrders(orderIds: java.lang.Iterable[OrderId], position: Optional[JPosition]): CompletableFuture[VEither[Problem, Void]] =
    execute(ResumeOrders(orderIds.asScala.toVector, position.toScala.map(_.asScala)))

  def removeOrdersWhenTerminated(orderIds: java.lang.Iterable[OrderId]): CompletableFuture[VEither[Problem, Void]] =
    execute(RemoveOrdersWhenTerminated(orderIds.asScala.toVector))

  def releaseEvents(until: EventId): CompletableFuture[VEither[Problem, Void]] =
    execute(ReleaseEvents(until))

  private def execute(command: ControllerCommand): CompletableFuture[VEither[Problem, Void]] =
    asScala.executeCommand(command)
      .mapt(_ => Void)
      .map(_.toVoidVavr)
      .runToFuture.asJava

  def executeCommand(command: JControllerCommand): CompletableFuture[VEither[Problem, ControllerCommand.Response]] =
    asScala.executeCommand(command.asScala)
      .map(_.map(o => (o: ControllerCommand.Response)).toVavr)
      .runToFuture
      .asJava

  def executeCommandJson(command: String): CompletableFuture[VEither[Problem, String]] =
    httpPostJson("/controller/api/command", command)

  def httpPostJson(uriTail: String, jsonString: String): CompletableFuture[VEither[Problem, String]] =
    asScala.httpPostJson(uriTail, jsonString)
      .map(_.toVavr)
      .runToFuture
      .asJava

  /** HTTP GET
    * @param uriTail path and query of the URI
    * @return `Either.Left(Problem)` or `Either.Right(json: String)`
    */
  def httpGetJson(uriTail: String): CompletableFuture[VEither[Problem, String]] =
    asScala.httpGetJson(uriTail)
      .map(_.toVavr)
      .runToFuture
      .asJava
}
