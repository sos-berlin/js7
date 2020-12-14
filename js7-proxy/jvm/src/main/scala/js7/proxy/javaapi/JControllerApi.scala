package js7.proxy.javaapi

import io.vavr.control.{Either => VEither}
import java.util.Objects.requireNonNull
import java.util.concurrent.CompletableFuture
import java.util.{Optional, OptionalLong}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.base.web.Uri
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.{AddOrdersResponse, CancelOrders, ReleaseEvents, RemoveOrdersWhenTerminated, ResumeOrder, ResumeOrders, SuspendOrders, TakeSnapshot}
import js7.data.cluster.ClusterSetting
import js7.data.event.{Event, EventId, JournalInfo}
import js7.data.item.VersionId
import js7.data.node.NodeId
import js7.data.order.OrderId
import js7.proxy.ControllerApi
import js7.proxy.data.ProxyEvent
import js7.proxy.javaapi.data.agent.JAgentRef
import js7.proxy.javaapi.data.command.{JCancelMode, JSuspendMode}
import js7.proxy.javaapi.data.common.JavaUtils.Void
import js7.proxy.javaapi.data.common.ReactorConverters._
import js7.proxy.javaapi.data.common.VavrConverters._
import js7.proxy.javaapi.data.controller.{JControllerCommand, JControllerState, JEventAndControllerState}
import js7.proxy.javaapi.data.item.{JUpdateItemOperation, JUpdateRepoOperation}
import js7.proxy.javaapi.data.order.{JFreshOrder, JHistoricOutcome}
import js7.proxy.javaapi.data.workflow.position.JPosition
import js7.proxy.javaapi.eventbus.{JControllerEventBus, JStandardEventBus}
import monix.execution.FutureUtils.Java8Extensions
import monix.execution.Scheduler
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

@javaApi
final class JControllerApi private[javaapi](val asScala: ControllerApi)(implicit scheduler: Scheduler)
{
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

  /** Update the Repo, i.e. add, change or deleteItem versioned items.
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
    *   - `jsonString` is the JSON-encoded `VersionedItem`, i.e. a Workflow or an AgentId.
    *     The item must include its id with `path` and `versionId`.
    *     The `versionId` must be the same as the first argument for `updateRepo`.
    *   - "PGP" or any supported signature type.
    *   - `signatureString` is the the signature of the UTF-8 encoded `jsonString`.
    *     {{{
    * signatureString = sign(jsonString.getBytes(UTF_8))
    *     }}}
    *
    * '''To deleteItem an item:'''
    * {{{
    * JUpdateRepoOperations.addOrReplace(ItemPath)
    * }}}
    *
    * `ItemPath` may be a [[js7.data.workflow.WorkflowPath]] or a [[js7.data.agent.AgentId]]
    * (both have a Java-compatible static factory method `of`).
    *
    * @param versionId `VersionId` of this new version
    * @param operations Stream of JUpdateRepoOperations
    *
    */
  @Deprecated
  @deprecated("Use updateItems")
  @Nonnull
  def updateRepo(
    @Nonnull versionId: VersionId,
    @Nonnull operations: Flux[JUpdateRepoOperation])
  : CompletableFuture[VEither[Problem, Void]] =
    asScala.updateRepo(requireNonNull(versionId), operations.asObservable.map(_.asScala))
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
    * Add a AgentRef and a versions with some signed workflows.
    * {{{

    * controllerApi.updateItems(
    *   Flux.fromStream(
    *     Stream.concat(
    *       Stream.of(JUpdateItemOperation.addOrReplace(agentRef)),
    *       Stream.concat(
    *         Stream.of(JUpdateItemOperation.addVersion(versionId)),
    *         workflowJsons.stream()
    *           .map(json -> JUpdateItemOperation.addOrReplace(sign(json))))))));

    * }}}
    * '''To add or replace a signed versioneditem:'''
    * {{{
    * JUpdateItemOperations.addOrReplace(
    *   SignedString.of(
    *     jsonString,
    *     "PGP"/*for example*/,
    *     signatureString)
    * }}}
    * `SignedString.of` requires three arguments:
    *   - `jsonString` is the JSON-encoded `VersionedItem`, i.e. a Workflow or an AgentId.
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
    * JUpdateItemOperations.deleteItem(ItemPath)
    * }}}
    *
    * `ItemPath` may be a [[js7.data.workflow.WorkflowPath]]
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
    cancelOrders(orderIds, JCancelMode.freshOrStarted)

  @Nonnull
  def cancelOrders(@Nonnull orderIds: java.lang.Iterable[OrderId], mode: JCancelMode): CompletableFuture[VEither[Problem, Void]] =
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
    execute(RemoveOrdersWhenTerminated(orderIds.asScala.toVector))

  @Nonnull
  def releaseEvents(until: EventId): CompletableFuture[VEither[Problem, Void]] =
    execute(ReleaseEvents(until))

  private def execute(command: ControllerCommand): CompletableFuture[VEither[Problem, Void]] =
    asScala.executeCommand(command)
      .mapt(_ => Void)
      .map(_.toVoidVavr)
      .runToFuture
      .asJava

  @Nonnull
  def takeSnapshot(): CompletableFuture[VEither[Problem, Void]] =
    asScala.executeCommand(TakeSnapshot)
      .mapt(_ => Void)
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
