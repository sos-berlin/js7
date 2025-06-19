package js7.proxy

import cats.effect.{IO, Resource, ResourceIO}
import fs2.Stream
import io.circe.{Json, JsonObject}
import js7.base.catsutils.CatsExtensions.{tryIt, untry}
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.Signed
import js7.base.eventbus.StandardEventBus
import js7.base.fs2utils.StreamExtensions.+:
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.monixutils.RefCountedResource
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient.{HttpException, isTemporaryUnreachable, liftProblem}
import js7.base.web.{HttpClient, Uri}
import js7.cluster.watch.api.ActiveClusterNodeSelector
import js7.controller.client.HttpControllerApi
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerCommand.Response.Accepted
import js7.data.controller.ControllerCommand.{AddOrder, AddOrders, ClusterAppointNodes, ReleaseEvents}
import js7.data.controller.ControllerState.*
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.{Event, EventId, JournalInfo}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, RemoveVersioned}
import js7.data.item.{ItemOperation, SignableItem, UnsignedSimpleItem, VersionId, VersionedItemPath}
import js7.data.node.NodeId
import js7.data.order.{FreshOrder, OrderId}
import js7.proxy.ControllerApi.*
import js7.proxy.JournaledProxy.EndOfEventStreamException
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.{EventAndState, ProxyEvent}
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable
import scala.concurrent.duration.Deadline.now
import scala.util.Failure

final class ControllerApi(
  val apisResource: ResourceIO[Nel[HttpControllerApi]],
  proxyConf: ProxyConf = ProxyConf.default,
  failWhenUnreachable: Boolean = false)
extends ControllerApiWithHttp:

  private val apiCache = new RefCountedResource(
    ActiveClusterNodeSelector.selectActiveNodeApi[HttpControllerApi](
      apisResource,
      proxyConf.recouplingStreamReaderConf.delayConf))

  protected def apiResource(implicit src: sourcecode.Enclosing) =
    apiCache.resource

  def stop: IO[Unit] =
    stop()

  def stop(dontLogout: Boolean = true): IO[Unit] =
    logger.debugIO:
      IO.whenA(dontLogout):
        apiCache.cachedValue
          .fold(IO.unit): api =>
            IO(api.clearSession())
      .productR:
        apiCache.release

  /** For testing (it's slow): wait for a condition in the running event stream. **/
  def when(predicate: EventAndState[Event, ControllerState] => Boolean)
  : IO[EventAndState[Event, ControllerState]] =
    CorrelId.bindIfEmpty:
      JournaledProxy.stream(apisResource, None, _ => (), proxyConf)
        .filter(predicate)
        .head.compile.last
        .map(_.getOrElse(throw new EndOfEventStreamException))

  /** Read events and state from Controller. */
  def eventAndStateStream(
    proxyEventBus: StandardEventBus[ProxyEvent] = new StandardEventBus,
    fromEventId: Option[EventId] = None)
  : Stream[IO, EventAndState[Event, ControllerState]] =
    // CorrelId.bind ???
    logger.debugStream:
      JournaledProxy.stream(apisResource, fromEventId, proxyEventBus.publish, proxyConf)

  def startProxy(
    proxyEventBus: StandardEventBus[ProxyEvent] = new StandardEventBus,
    eventBus: JournaledStateEventBus[ControllerState] = new JournaledStateEventBus[ControllerState])
  : IO[ControllerProxy] =
    logger.traceIO:
      proxyResource(proxyEventBus, eventBus)
        .allocated // Caller must stop the ControllerProxy
        .map(_._1)

  def proxyResource(
    proxyEventBus: StandardEventBus[ProxyEvent] = new StandardEventBus,
    eventBus: JournaledStateEventBus[ControllerState] = new JournaledStateEventBus[ControllerState])
  : ResourceIO[ControllerProxy] =
    /*CorrelId.bindIfEmpty???*/(logger.debugResource:
      ControllerProxy.resource(this, apisResource, proxyEventBus, eventBus, proxyConf))

  def clusterAppointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId)
  : IO[Checked[Accepted]] =
    executeCommand(ClusterAppointNodes(idToUri, activeId))

  def updateRepo(
    versionId: VersionId,
    signedItems: immutable.Iterable[Signed[SignableItem]] = Nil,
    delete: immutable.Iterable[VersionedItemPath] = Nil)
  : IO[Checked[Completed]] =
    updateItems(AddVersion(versionId) +: (
      Stream.iterable(signedItems).map(o => AddOrChangeSigned(o.signedString)) ++
        Stream.iterable(delete).map(RemoveVersioned.apply)))

  def updateUnsignedSimpleItems(items: Iterable[UnsignedSimpleItem]): IO[Checked[Completed]] =
    updateItems:
      Stream.iterable(items) map ItemOperation.AddOrChangeSimple.apply

  def updateSignedItems(items: Iterable[Signed[SignableItem]], versionId: Option[VersionId] = None)
  : IO[Checked[Completed]] =
    updateItems:
      Stream.iterable(versionId).map(AddVersion(_)) ++
        Stream.iterable(items).map(o => ItemOperation.AddOrChangeSigned(o.signedString))

  def updateItems(operations: Stream[IO, ItemOperation]): IO[Checked[Completed]] =
    logger.debugIO:
      untilReachable:
        _.postStream[ItemOperation, JsonObject](
          "controller/api/item",
          operations)
      .map(_.map((_: JsonObject) => Completed))

  def addOrders(orders: Stream[IO, FreshOrder]): IO[Checked[AddOrders.Response]] =
    logger.debugIO:
      untilReachable(_.postStream[FreshOrder, Json]("controller/api/order", orders))
        .map(_.flatMap(_.checkedAs[AddOrders.Response]))

  /** @return true if added, otherwise false because of duplicate OrderId. */
  def addOrder(order: FreshOrder): IO[Checked[Boolean]] =
    executeCommand(AddOrder(order))
      .map(_.map(o => !o.ignoredBecauseDuplicate))

  def deleteOrdersWhenTerminated(orderIds: Stream[IO, OrderId])
  : IO[Checked[ControllerCommand.Response.Accepted]] =
    logger.debugIO:
      untilReachable:
        _.postStream[OrderId, Json](
          "controller/api/order/DeleteOrdersWhenTerminated",
          orderIds)
      .map(_.flatMap(_
        .checkedAs[ControllerCommand.Response]
        .map(_.asInstanceOf[ControllerCommand.Response.Accepted])))

  def releaseEvents(eventId: EventId): IO[Checked[Completed]] =
    executeCommand(ReleaseEvents(eventId))
      .map(_.map((_: ControllerCommand.Response.Accepted) => Completed))

  def executeCommand[C <: ControllerCommand](command: C): IO[Checked[command.Response]] =
    logger.debugIO(s"executeCommand ${command.toShortString}"):
      untilReachable(_.executeCommand(command))

  //def executeAgentCommand(agentPath: AgentPath, command: AgentCommand)
  //: IO[Checked[command.Response]] =
  //  logger.debugIO("executeAgentCommand", command.toShortString)(
  //    untilReachable(_.executeAgentCommand(agentPath, command)))

  def journalInfo: IO[Checked[JournalInfo]] =
    logger.debugIO:
      untilReachable(_.journalInfo)

  def controllerState: IO[Checked[ControllerState]] =
    logger.debugIO:
      untilReachable(_.snapshot())

  /** Fetch the WHOLE ControllerState only to select the ClusterState. */
  @TestOnly
  def clusterState: IO[Checked[ClusterState]] =
    controllerState.map(_.map(_.clusterState))

  private def untilReachable[A](body: HttpControllerApi => IO[A]): IO[Checked[A]] =
    CorrelId.bindIfEmpty:
      if !failWhenUnreachable then
        untilReachable1(body)
      else
        liftProblem:
          apiResource.use: api =>
            api.retryIfSessionLost:
              body(api)

  private def untilReachable1[A](body: HttpControllerApi => IO[A]): IO[Checked[A]] =
    CorrelId.bindIfEmpty(IO.defer:
      // TODO Similar to SessionApi.retryUntilReachable
      val delays = SessionApi.defaultLoginDelays()
      var warned = now - 1.h
      apiResource.use: api =>
        api.retryIfSessionLost(body(api))
          .tryIt.map:
            case Failure(t: HttpException) if t.statusInt == 503/*Service unavailable*/ =>
              Failure(t)  // Trigger onErrorRestartLoop
            case o => HttpClient.failureToChecked(o)
          .untry
      .onErrorRestartLoop(()):
        case (t, _, retry)
          if isTemporaryUnreachable(t) && delays.hasNext =>
          if warned.elapsed >= 60.s then
            logger.warn(t.toStringWithCauses)
            warned = now
          else
            logger.debug(t.toStringWithCauses)
          apiCache.clear *>
            IO.sleep(delays.next()) *>
            retry(())

        case (t, _, _) => IO.raiseError(t))


object ControllerApi:
  private val logger = Logger[this.type]

  def resource(
    apisResource: ResourceIO[Nel[HttpControllerApi]],
    proxyConf: ProxyConf = ProxyConf.default)
  : ResourceIO[ControllerApi] =
    Resource.make(IO { new ControllerApi(apisResource, proxyConf) })(_.stop)
