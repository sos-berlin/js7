package js7.proxy.javaapi

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import io.vavr.control.Either as VEither
import java.time.Instant
import java.util.Objects.requireNonNull
import java.util.concurrent.CompletableFuture
import java.util.function.Consumer
import java.util.{Optional, OptionalLong}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.log.CorrelId
import js7.base.monixutils.AsyncVariable
import js7.base.problem.Problem
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatchService
import js7.data.board.{BoardPath, NoticeId, NoticeKey}
import js7.data.cluster.ClusterWatchId
import js7.data.cluster.ClusterWatchProblems.ClusterNodeLossNotConfirmedProblem
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{AddOrdersResponse, CancelOrders, ReleaseEvents, ResumeOrder, ResumeOrders, SuspendOrders, TakeSnapshot}
import js7.data.event.{Event, EventId, JournalInfo}
import js7.data.node.NodeId
import js7.data.order.OrderId
import js7.data_for_java.command.{JCancellationMode, JSuspensionMode}
import js7.data_for_java.common.JavaUtils.Void
import js7.data_for_java.controller.{JControllerCommand, JControllerState}
import js7.data_for_java.item.JUpdateItemOperation
import js7.data_for_java.order.{JFreshOrder, JHistoryOperation}
import js7.data_for_java.reactor.ReactorConverters.*
import js7.data_for_java.vavr.VavrConverters.*
import js7.data_for_java.workflow.position.JPosition
import js7.proxy.ControllerApi
import js7.proxy.data.event.ProxyEvent
import js7.proxy.javaapi.data.controller.JEventAndControllerState
import js7.proxy.javaapi.eventbus.{JControllerEventBus, JStandardEventBus}
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

@javaApi
final class JControllerApi(val asScala: ControllerApi, val config: Config)
  (using ioRuntime: IORuntime):

  private val clusterWatchService = AsyncVariable[Option[Allocated[IO, ClusterWatchService]]](None)
  protected val prefetch = config.getInt("js7.web.client.prefetch")

  def stop: CompletableFuture[Void] =
    runIO(
      stopClusterWatch_
        .*>(asScala.stop)
        .as(Void))

  /** Fetch event stream from Controller. */
  @Nonnull
  def eventFlux(@Nonnull proxyEventBus: JStandardEventBus[ProxyEvent])
  : Flux[JEventAndControllerState[Event]] =
    eventFlux(requireNonNull(proxyEventBus), OptionalLong.empty())

  /** Fetch event stream from Controller. */
  @Nonnull
  def eventFlux(@Nonnull after: OptionalLong/*EventId*/): Flux[JEventAndControllerState[Event]] =
    eventFlux(new JStandardEventBus, after = after)

  /** Fetch event stream from Controller. */
  @Nonnull
  def eventFlux(
    @Nonnull proxyEventBus: JStandardEventBus[ProxyEvent],
    @Nonnull after: OptionalLong/*EventId*/)
  : Flux[JEventAndControllerState[Event]] =
    asScala
      .eventAndStateStream(proxyEventBus.asScala, after.toScala)
      .map(JEventAndControllerState.apply)
      .asFlux

  @Nonnull
  def startProxy(): CompletableFuture[JControllerProxy] =
    startProxy(new JStandardEventBus[ProxyEvent])

  @Nonnull
  def startProxy(@Nonnull proxyEventBus: JStandardEventBus[ProxyEvent])
  : CompletableFuture[JControllerProxy] =
    startProxy(proxyEventBus, new JControllerEventBus)

  /** Starts a `JControllerProxy`.
    * After use, stop it with `JControllerProxy.stop()`. */
  @Nonnull
  def startProxy(
    @Nonnull proxyEventBus: JStandardEventBus[ProxyEvent],
    @Nonnull controllerEventBus: JControllerEventBus)
  : CompletableFuture[JControllerProxy] =
    runIO(asScala
      .startProxy(proxyEventBus.asScala, controllerEventBus.asScala)
      .map(new JControllerProxy(_, this, controllerEventBus)))

  @Nonnull
  def clusterAppointNodes(
    @Nonnull idToUri: java.util.Map[NodeId, Uri],
    @Nonnull activeId: NodeId)
  : CompletableFuture[VEither[Problem, Void]] =
    requireNonNull(activeId)
    runIO(asScala
      .clusterAppointNodes(idToUri.asScala.toMap, activeId)
      .map(_.toVoidVavr))

  /** Update the Items, i.e. add, change or remove/delete simple or versioned items.
    *
    * The `JUpdateItemOperation.addVersion` adds a version.
    * This operation must be given exactly only if any versioned times are concerned.
    *
    * All other `JUpdateItemOperation`s add/replace or remove/delete items.
    *
    * '''Example'''
    *
    * Add AgentRefs and a version with some signed workflows.
    * {{{
    *
    * import static js7.proxy.javaapi.data.item.JUpdateItemOperation.addOrChange;
    * import static js7.proxy.javaapi.data.item.JUpdateItemOperation.addVersion;
    * import static js7.proxy.javaapi.data.item.JUpdateItemOperation.removeVersioned;
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
    * '''To remove an item:'''
    * {{{
    * JUpdateItemOperations.removeVersioned(VersionedItemPath)
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
    runIO(asScala
      .updateItems(operations.asFs2Stream(bufferSize = prefetch).map(_.asScala))
      .map(_.toVoidVavr))

  /** @return true iff added, false iff not added because of duplicate OrderId. */
  @Nonnull
  def addOrder(@Nonnull order: JFreshOrder): CompletableFuture[VEither[Problem, java.lang.Boolean]] =
    runIO(asScala
      .addOrder(order.asScala)
      .map(_.map(o => java.lang.Boolean.valueOf(o)).toVavr))

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
  def addOrders(@Nonnull orders: Flux[JFreshOrder])
  : CompletableFuture[VEither[Problem, AddOrdersResponse]] =
    runIO(asScala
      .addOrders(orders.asFs2Stream(bufferSize = prefetch).map(_.asScala))
      .map(_.toVavr))

  @Nonnull
  def cancelOrders(@Nonnull orderIds: java.lang.Iterable[OrderId])
  : CompletableFuture[VEither[Problem, Void]] =
    cancelOrders(orderIds, JCancellationMode.freshOrStarted)

  @Nonnull
  def cancelOrders(@Nonnull orderIds: java.lang.Iterable[OrderId], mode: JCancellationMode)
  : CompletableFuture[VEither[Problem, Void]] =
    execute(CancelOrders(orderIds.asScala.toVector, mode.asScala))

  @Nonnull
  def suspendOrders(@Nonnull orderIds: java.lang.Iterable[OrderId])
  : CompletableFuture[VEither[Problem, Void]] =
    execute(SuspendOrders(orderIds.asScala.toVector))

  @Nonnull
  def suspendOrders(@Nonnull orderIds: java.lang.Iterable[OrderId], mode: JSuspensionMode)
  : CompletableFuture[VEither[Problem, Void]] =
    execute(SuspendOrders(orderIds.asScala.toVector, mode.asScala))

  @Nonnull
  def resumeOrder(
    @Nonnull orderId: OrderId,
    @Nonnull position: Optional[JPosition],
    @Nonnull historyOperations: java.util.List[JHistoryOperation],
    asSucceeded: Boolean,
    @Nonnull restartKilledJob: Optional[Boolean])
  : CompletableFuture[VEither[Problem, Void]] =
    execute(ResumeOrder(
      orderId.nn,
      position.toScala.map(_.asScala),
      historyOperations.asScala.view.map(_.asScala).toVector,
      asSucceeded = asSucceeded,
      restartKilledJob = restartKilledJob.toScala))

  @Nonnull
  def resumeOrders(
    @Nonnull orderIds: java.lang.Iterable[OrderId],
    asSucceeded: Boolean,
    @Nonnull restartKilledJob: Optional[Boolean])
  : CompletableFuture[VEither[Problem, Void]] =
    execute(ResumeOrders(
      orderIds.asScala.toVector,
      asSucceeded = asSucceeded,
      restartKilledJob = restartKilledJob.toScala))

  @Nonnull
  def deleteOrdersWhenTerminated(@Nonnull orderIds: java.lang.Iterable[OrderId])
  : CompletableFuture[VEither[Problem, Void]] =
    deleteOrdersWhenTerminated(Flux.fromIterable(orderIds))

  @Nonnull
  def deleteOrdersWhenTerminated(@Nonnull orderIds: Flux[OrderId])
  : CompletableFuture[VEither[Problem, Void]] =
    runIO(asScala
      .deleteOrdersWhenTerminated(orderIds.asFs2Stream(bufferSize = prefetch))
      .map(_.toVoidVavr))

  @Nonnull
  @Deprecated @deprecated("Use postGlobalNotice", "v2.7.4")
  def postNotice(
    @Nonnull boardPath: BoardPath,
    @Nonnull noticeKey: NoticeKey,
    @Nonnull endOfLife: Optional[Instant])
  : CompletableFuture[VEither[Problem, Void]] =
    postGlobalNotice(boardPath, noticeKey, endOfLife)

  @Nonnull
  def postGlobalNotice(
    @Nonnull boardPath: BoardPath,
    @Nonnull noticeKey: NoticeKey,
    @Nonnull endOfLife: Optional[Instant])
  : CompletableFuture[VEither[Problem, Void]] =
    execute:
      JControllerCommand.postGlobalNotice(boardPath, noticeKey, endOfLife)
        .asScala

  @Nonnull
  def postNotice(
    @Nonnull noticeId: NoticeId,
    @Nonnull endOfLife: Optional[Instant])
  : CompletableFuture[VEither[Problem, Void]] =
    execute:
      JControllerCommand.postNotice(noticeId, endOfLife)
        .asScala

  @Nonnull
  def releaseEvents(until: EventId): CompletableFuture[VEither[Problem, Void]] =
    execute(ReleaseEvents(until))

  private def execute(command: ControllerCommand): CompletableFuture[VEither[Problem, Void]] =
    runIO(asScala
      .executeCommand(command)
      .map(_.toVoidVavr))

  @Nonnull
  def takeSnapshot(): CompletableFuture[VEither[Problem, Void]] =
    runIO(asScala
      .executeCommand(TakeSnapshot)
      .map(_.toVoidVavr))

  @Nonnull
  def executeCommand(@Nonnull command: JControllerCommand)
  : CompletableFuture[VEither[Problem, ControllerCommand.Response]] =
    runIO(asScala
      .executeCommand(command.asScala)
      .map(_.map(o => (o: ControllerCommand.Response)).toVavr))

  @deprecated("Use executeCommand", "v2.6.1")
  @Deprecated
  @Nonnull
  def executeCommandJson(@Nonnull command: String): CompletableFuture[VEither[Problem, String]] =
    httpPostJson("/controller/api/command", requireNonNull(command))

  @Nonnull
  def httpPostJson(
    @Nonnull uriTail: String,
    @Nonnull jsonString: String)
  : CompletableFuture[VEither[Problem, String]] =
    runIO(asScala
      .httpPostJson(requireNonNull(uriTail), requireNonNull(jsonString))
      .map(_.toVavr))

  /** HTTP GET
    * @param uriTail path and query of the URI
    * @return `Either.Left(Problem)` or `Either.Right(json: String)`
    */
  @Nonnull
  def httpGetJson(@Nonnull uriTail: String): CompletableFuture[VEither[Problem, String]] =
    runIO(asScala
      .httpGetJson(requireNonNull(uriTail))
      .map(_.toVavr))

  @Nonnull
  def journalInfo: CompletableFuture[VEither[Problem, JournalInfo]] =
    runIO(asScala
      .journalInfo
      .map(_.toVavr))

  /** Fetch the maybe very big JournalState. */
  @Nonnull
  def controllerState: CompletableFuture[VEither[Problem, JControllerState]] =
    runIO(asScala
      .controllerState
      .map(_ map JControllerState.apply)
      .map(_.toVavr))

  /** For testing (it's slow): wait for a condition in the running event stream. **/
  @Nonnull
  def when(@Nonnull predicate: JEventAndControllerState[Event] => Boolean)
  : CompletableFuture[JEventAndControllerState[Event]] =
    requireNonNull(predicate)
    runIO(asScala
      .when(es => predicate(JEventAndControllerState(es)))
      .map(JEventAndControllerState.apply))

  @Nonnull
  def runClusterWatch(@Nonnull clusterWatchId: ClusterWatchId): CompletableFuture[Void] =
    runIO:
      startClusterWatch_(clusterWatchId, _ => ())
        .flatTap(_.untilStopped)
        .as(Void)

  @Nonnull
  def startClusterWatch(
    @Nonnull clusterWatchId: ClusterWatchId,
    @Nonnull onUndecidableClusterNodeLoss: Consumer[ClusterNodeLossNotConfirmedProblem])
  : CompletableFuture[ClusterWatchService] =
    runIO:
      startClusterWatch_(clusterWatchId, onUndecidableClusterNodeLoss)

  private def startClusterWatch_(
    clusterWatchId: ClusterWatchId,
    onUndecidableClusterNodeLoss: Consumer[ClusterNodeLossNotConfirmedProblem])
  : IO[ClusterWatchService] =
    clusterWatchService
      .update:
        case Some(service) => IO.some(service)
        case None =>
          ClusterWatchService
            .resource(clusterWatchId, asScala.apisResource, config,
              onUndecidableClusterNodeLoss = {
                case Some(prblm) => IO(onUndecidableClusterNodeLoss.accept(prblm))
                case None => IO.unit
              })
            .toAllocated
            .map(Some(_))
      .map(_.get.allocatedThing)

  @Nonnull
  def stopClusterWatch: CompletableFuture[Void] =
    runIO(
      stopClusterWatch_.as(Void))

  private def stopClusterWatch_ : IO[Unit] =
    clusterWatchService
      .update(_.fold(IO.none)(_.release.as(None)))
      .void

  @javaApi
  def manuallyConfirmNodeLoss(lostNodeId: NodeId, confirmer: String)
  : CompletableFuture[VEither[Problem, Void]] =
    runIO:
      clusterWatchService.value
        .flatMap:
          case None => IO.left(Problem("No ClusterWatchService"))
          case Some(allo) => allo.allocatedThing.manuallyConfirmNodeLoss(lostNodeId, confirmer)
        .map(_.toVoidVavr)

  private def runIO[A](io: IO[A]): CompletableFuture[A] =
    CorrelId.bindNew(io).unsafeToCompletableFuture()
