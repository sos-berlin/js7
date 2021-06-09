package js7.proxy.javaapi

import io.vavr.control.{Either => VEither}
import java.util.Objects.requireNonNull
import java.util.concurrent.CompletableFuture
import java.util.{Optional, OptionalLong}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data.cluster.ClusterSetting
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{AddOrdersResponse, CancelOrders, ReleaseEvents, ResumeOrder, ResumeOrders, SuspendOrders, TakeSnapshot}
import js7.data.event.{Event, EventId, JournalInfo}
import js7.data.node.NodeId
import js7.data.order.OrderId
import js7.data_for_java.agent.JAgentRef
import js7.data_for_java.command.{JCancellationMode, JSuspendMode}
import js7.data_for_java.controller.{JControllerCommand, JControllerState}
import js7.data_for_java.item.JUpdateItemOperation
import js7.data_for_java.order.{JFreshOrder, JHistoricOutcome}
import js7.data_for_java.reactor.ReactorConverters._
import js7.data_for_java.vavr.VavrConverters._
import js7.data_for_java.workflow.position.JPosition
import js7.proxy.ControllerApi
import js7.proxy.data.event.ProxyEvent
import js7.proxy.javaapi.data.controller.JEventAndControllerState
import js7.proxy.javaapi.eventbus.{JControllerEventBus, JStandardEventBus}
import monix.execution.FutureUtils.Java8Extensions
import monix.execution.Scheduler
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

@javaApi
final class JControllerApi(val asScala: ControllerApi)(implicit scheduler: Scheduler)
extends AutoCloseable
{
  def close() =
    asScala.close()

  /** Fetch event stream from Controller. */
  @Nonnull
  def eventFlux(@Nonnull proxyEventBus: JStandardEventBus[ProxyEvent]): Flux[JEventAndControllerState[Event]] =
    eventFlux(requireNonNull(proxyEventBus), OptionalLong.empty())

  /** Fetch event stream from Controller. */
  @Nonnull
  def eventFlux(
    @Nonnull proxyEventBus: JStandardEventBus[ProxyEvent],
    after: OptionalLong/*EventId*/)
  : Flux[JEventAndControllerState[Event]] =
    asScala.eventAndStateObservable(proxyEventBus.asScala, after.toScala)
      .map(JEventAndControllerState.apply)
      .asFlux

  @Nonnull
  def startProxy(): CompletableFuture[JControllerProxy] =
    startProxy(new JStandardEventBus[ProxyEvent])

  @Nonnull
  def startProxy(@Nonnull proxyEventBus: JStandardEventBus[ProxyEvent]): CompletableFuture[JControllerProxy] =
    startProxy(proxyEventBus, new JControllerEventBus)

  /** Starts a `JControllerProxy`.
    * After use, stop it with `JControllerProxy.stop()`. */
  @Nonnull
  def startProxy(
    @Nonnull proxyEventBus: JStandardEventBus[ProxyEvent],
    @Nonnull controllerEventBus: JControllerEventBus)
  : CompletableFuture[JControllerProxy] =
    asScala.startProxy(proxyEventBus.asScala, controllerEventBus.asScala)
      .map(new JControllerProxy(_, this, controllerEventBus))
      .runToFuture
      .asJava

  @Nonnull
  def clusterAppointNodes(
    @Nonnull idToUri: java.util.Map[NodeId, Uri],
    @Nonnull activeId: NodeId,
    @Nonnull clusterWatches: java.util.List[ClusterSetting.Watch])
  : CompletableFuture[VEither[Problem, Void]] = {
    requireNonNull(activeId)
    asScala.clusterAppointNodes(idToUri.asScala.toMap, activeId, clusterWatches.asScala.toVector)
      .map(_.toVoidVavr)
      .runToFuture
      .asJava
  }

  @Deprecated
  @deprecated("Use updateItems", "2020-12-11")
  @Nonnull
  def updateAgentRefs(@Nonnull agentRefs: java.util.List[JAgentRef]): CompletableFuture[VEither[Problem, Void]] =
    asScala.updateAgentRefs(agentRefs.asScala.map(_.asScala).toVector)
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  /** Update the Items, i.e. add, change or deleteItem simple or versioned items.
    *
    * The `JUpdateItemOperation.addVersion` adds a version.
    * This operation must be given exactly only if any versioned times are concerned.
    *
    * All other `JUpdateItemOperation`s add/replace or deleteItem items.
    *
    * '''Example'''
    *
    * Add AgentRefs and a version with some signed workflows.
    * {{{
    *
    * import static js7.proxy.javaapi.data.item.JUpdateItemOperation.addOrChange;
    * import static js7.proxy.javaapi.data.item.JUpdateItemOperation.addVersion;
    * import static js7.proxy.javaapi.data.item.JUpdateItemOperation.deleteItem;
    *
    * controllerApi.updateItems(
    *   Flux.concat(
    *     Flux.fromIterable(agentRefs)
    *       .map(item -> addOrChange(item)),
    *     Flux.just(addVersion(versionId)),
    *     Flux.fromIterable(workflowJsons)
    *       .map(json -> addOrChange(sign(json)))));
    *
    * }}}
    * '''To add or replace a signed versioneditem:'''
    * {{{
    * JUpdateItemOperations.addOrChange(
    *   SignedString.of(
    *     jsonString,
    *     "PGP"/*for example*/,
    *     signatureString)
    * }}}
    * `SignedString.of` requires three arguments:
    *   - `jsonString` is the JSON-encoded `VersionedItem`, i.e. a Workflow or an AgentPath.
    *     The item must include its id with `path` and `versionId`.
    *     The `versionId` must be the same as the one `JUpdateItemOperations.addVersion`.
    *   - "PGP" or any supported signature type.
    *   - `signatureString` is the the signature of the UTF-8 encoded `jsonString`.
    *     {{{
    * signatureString = sign(jsonString.getBytes(UTF_8))
    *     }}}
    *
    * '''To deleteItem an item:'''
    * {{{
    * JUpdateItemOperations.deleteItem(VersionedItemPath)
    * }}}
    *
    * `VersionedItemPath` may be a [[js7.data.workflow.WorkflowPath]]
    * (it has a Java-compatible static factory method `of`).
    *
    * @param operations Stream of JUpdateItemOperations
    *
    */
  @Nonnull
  def updateItems(@Nonnull operations: Flux[JUpdateItemOperation])
  : CompletableFuture[VEither[Problem, Void]] =
    asScala.updateItems(operations.asObservable.map(_.asScala))
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  /** @return true iff added, false iff not added because of duplicate OrderId. */
  @Nonnull
  def addOrder(@Nonnull order: JFreshOrder): CompletableFuture[VEither[Problem, java.lang.Boolean]] =
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
  @Nonnull
  def addOrders(@Nonnull orders: Flux[JFreshOrder]): CompletableFuture[VEither[Problem, AddOrdersResponse]] =
    asScala.addOrders(orders.asObservable.map(_.asScala))
      .map(_.toVavr)
      .runToFuture
      .asJava

  @Nonnull
  def cancelOrders(@Nonnull orderIds: java.lang.Iterable[OrderId]): CompletableFuture[VEither[Problem, Void]] =
    cancelOrders(orderIds, JCancellationMode.freshOrStarted)

  @Nonnull
  def cancelOrders(@Nonnull orderIds: java.lang.Iterable[OrderId], mode: JCancellationMode): CompletableFuture[VEither[Problem, Void]] =
    execute(CancelOrders(orderIds.asScala.toVector, mode.asScala))

  @Nonnull
  def suspendOrders(@Nonnull orderIds: java.lang.Iterable[OrderId]): CompletableFuture[VEither[Problem, Void]] =
    execute(SuspendOrders(orderIds.asScala.toVector))

  @Nonnull
  def suspendOrders(@Nonnull orderIds: java.lang.Iterable[OrderId], mode: JSuspendMode): CompletableFuture[VEither[Problem, Void]] =
    execute(SuspendOrders(orderIds.asScala.toVector, mode.asScala))

  @Nonnull
  def resumeOrder(
    @Nonnull orderId: OrderId,
    @Nonnull position: Optional[JPosition],
    @Nonnull historyOutcomes: Optional[java.util.List[JHistoricOutcome]])
  : CompletableFuture[VEither[Problem, Void]] =
    execute(ResumeOrder(
      requireNonNull(orderId),
      position.toScala.map(_.asScala),
      historyOutcomes.toScala.map(_.asScala.toVector).map(_.map(_.asScala))))

  @Nonnull
  def resumeOrders(@Nonnull orderIds: java.lang.Iterable[OrderId]): CompletableFuture[VEither[Problem, Void]] =
    execute(ResumeOrders(orderIds.asScala.toVector))

  @Nonnull
  def removeOrdersWhenTerminated(@Nonnull orderIds: java.lang.Iterable[OrderId]): CompletableFuture[VEither[Problem, Void]] =
    removeOrdersWhenTerminated(Flux.fromIterable(orderIds))

  @Nonnull
  def removeOrdersWhenTerminated(@Nonnull orderIds: Flux[OrderId]): CompletableFuture[VEither[Problem, Void]] =
    asScala.removeOrdersWhenTerminated(orderIds.asObservable)
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  @Nonnull
  def releaseEvents(until: EventId): CompletableFuture[VEither[Problem, Void]] =
    execute(ReleaseEvents(until))

  private def execute(command: ControllerCommand): CompletableFuture[VEither[Problem, Void]] =
    asScala.executeCommand(command)
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  @Nonnull
  def takeSnapshot(): CompletableFuture[VEither[Problem, Void]] =
    asScala.executeCommand(TakeSnapshot)
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  @Nonnull
  def executeCommand(@Nonnull command: JControllerCommand): CompletableFuture[VEither[Problem, ControllerCommand.Response]] =
    asScala.executeCommand(command.asScala)
      .map(_.map(o => (o: ControllerCommand.Response)).toVavr)
      .runToFuture
      .asJava

  @Nonnull
  def executeCommandJson(@Nonnull command: String): CompletableFuture[VEither[Problem, String]] =
    httpPostJson("/controller/api/command", requireNonNull(command))

  @Nonnull
  def httpPostJson(
    @Nonnull uriTail: String,
    @Nonnull jsonString: String)
  : CompletableFuture[VEither[Problem, String]] =
    asScala.httpPostJson(requireNonNull(uriTail), requireNonNull(jsonString))
      .map(_.toVavr)
      .runToFuture
      .asJava

  /** HTTP GET
    * @param uriTail path and query of the URI
    * @return `Either.Left(Problem)` or `Either.Right(json: String)`
    */
  @Nonnull
  def httpGetJson(@Nonnull uriTail: String): CompletableFuture[VEither[Problem, String]] =
    asScala.httpGetJson(requireNonNull(uriTail))
      .map(_.toVavr)
      .runToFuture
      .asJava

  @Nonnull
  def journalInfo: CompletableFuture[VEither[Problem, JournalInfo]] =
    asScala.journalInfo
      .map(_.toVavr)
      .runToFuture
      .asJava

  /** Fetch the maybe very big JournalState. */
  @Nonnull
  def controllerState: CompletableFuture[VEither[Problem, JControllerState]] =
    asScala.controllerState
      .map(_ map JControllerState.apply)
      .map(_.toVavr)
      .runToFuture
      .asJava

  /** For testing (it's slow): wait for a condition in the running event stream. **/
  @Nonnull
  def when(@Nonnull predicate: JEventAndControllerState[Event] => Boolean): CompletableFuture[JEventAndControllerState[Event]] = {
    requireNonNull(predicate)
    asScala.when(es => predicate(JEventAndControllerState(es)))
      .map(JEventAndControllerState.apply)
      .runToFuture
      .asJava
  }
}
