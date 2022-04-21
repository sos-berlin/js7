package js7.proxy

import cats.effect.Resource
import io.circe.{Json, JsonObject}
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.Signed
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.monixutils.RefCountedResource
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.HttpClient.HttpException
import js7.base.web.{HttpClient, Uri}
import js7.controller.client.HttpControllerApi
import js7.data.cluster.ClusterSetting
import js7.data.controller.ControllerCommand.Response.Accepted
import js7.data.controller.ControllerCommand.{AddOrder, AddOrders, ReleaseEvents}
import js7.data.controller.ControllerState._
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.{Event, EventId, JournalInfo}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, RemoveVersioned}
import js7.data.item.{ItemOperation, SignableItem, UnsignedSimpleItem, VersionId, VersionedItemPath}
import js7.data.node.NodeId
import js7.data.order.{FreshOrder, OrderId}
import js7.proxy.ControllerApi._
import js7.proxy.JournaledProxy.EndOfEventStreamException
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.{EventAndState, ProxyEvent}
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.immutable
import scala.concurrent.duration.Deadline.now
import scala.util.Failure

final class ControllerApi(
  apiResources: Seq[Resource[Task, HttpControllerApi]],
  proxyConf: ProxyConf = ProxyConf.default)
extends ControllerApiWithHttp
{
  if (apiResources.isEmpty) throw new IllegalArgumentException("apiResources must not be empty")

  private val apiCache = new RefCountedResource(
    JournaledProxy.selectActiveNodeApi(
      apiResources,
      onCouplingError = _ => t => SessionApi.logError(t).void,
      proxyConf))

  protected val apiResource = apiCache.resource

  def stop: Task[Unit] =
    apiCache.release

  /** For testing (it's slow): wait for a condition in the running event stream. **/
  def when(predicate: EventAndState[Event, ControllerState] => Boolean): Task[EventAndState[Event, ControllerState]] =
    JournaledProxy.observable(apiResources, None, _ => (), proxyConf)
      .filter(predicate)
      .headOptionL
      .map(_.getOrElse(throw new EndOfEventStreamException))

  /** Read events and state from Controller. */
  def eventAndStateObservable(
    proxyEventBus: StandardEventBus[ProxyEvent] = new StandardEventBus,
    fromEventId: Option[EventId] = None)
  : Observable[EventAndState[Event, ControllerState]] =
    JournaledProxy.observable(apiResources, fromEventId, proxyEventBus.publish, proxyConf)

  def startProxy(
    proxyEventBus: StandardEventBus[ProxyEvent] = new StandardEventBus,
    eventBus: JournaledStateEventBus[ControllerState] = new JournaledStateEventBus[ControllerState])
  : Task[ControllerProxy] =
    ControllerProxy.start(this, apiResources, proxyEventBus, eventBus, proxyConf)

  def clusterAppointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId, clusterWatches: Seq[ClusterSetting.Watch])
  : Task[Checked[Accepted]] =
    executeCommand(ControllerCommand.ClusterAppointNodes(idToUri, activeId, clusterWatches))

  def updateRepo(
    versionId: VersionId,
    signedItems: immutable.Iterable[Signed[SignableItem]] = Nil,
    delete: immutable.Iterable[VersionedItemPath] = Nil)
  : Task[Checked[Completed]] =
    updateItems(AddVersion(versionId) +: (
      Observable.fromIterable(signedItems).map(o => AddOrChangeSigned(o.signedString)) ++
        Observable.fromIterable(delete).map(RemoveVersioned.apply)))

  def updateUnsignedSimpleItems(items: Iterable[UnsignedSimpleItem]): Task[Checked[Completed]] =
    updateItems(
      Observable.fromIterable(items) map ItemOperation.AddOrChangeSimple.apply)

  def updateSignedItems(items: Iterable[Signed[SignableItem]], versionId: Option[VersionId] = None)
  : Task[Checked[Completed]] =
    updateItems(
      Observable.fromIterable(versionId).map(AddVersion(_)) ++
        Observable.fromIterable(items).map(o => ItemOperation.AddOrChangeSigned(o.signedString)))

  def updateItems(operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    untilReachable(_
      .postObservable[ItemOperation, JsonObject](
        "controller/api/item",
        operations)
    ).map(_.map((_: JsonObject) => Completed))

  def addOrders(orders: Observable[FreshOrder]): Task[Checked[AddOrders.Response]] =
    untilReachable(_.postObservable[FreshOrder, Json]("controller/api/order", orders))
      .map(_.flatMap(_.checkedAs[AddOrders.Response]))

  /** @return true if added, otherwise false because of duplicate OrderId. */
  def addOrder(order: FreshOrder): Task[Checked[Boolean]] =
    executeCommand(AddOrder(order))
      .map(_.map(o => !o.ignoredBecauseDuplicate))

  def deleteOrdersWhenTerminated(orderIds: Observable[OrderId])
  : Task[Checked[ControllerCommand.Response.Accepted]] =
    untilReachable(_
      .postObservable[OrderId, Json](
        "controller/api/order/DeleteOrdersWhenTerminated",
        orderIds))
      .map(_.flatMap(_
        .checkedAs[ControllerCommand.Response]
        .map(_.asInstanceOf[ControllerCommand.Response.Accepted])))

  def releaseEvents(eventId: EventId): Task[Checked[Completed]] =
    executeCommand(ReleaseEvents(eventId))
      .mapt((_: ControllerCommand.Response.Accepted) => Completed)

  def executeCommand(command: ControllerCommand): Task[Checked[command.Response]] =
    untilReachable(_.executeCommand(command))

  def journalInfo: Task[Checked[JournalInfo]] =
    untilReachable(_.journalInfo)

  def controllerState: Task[Checked[ControllerState]] =
    untilReachable(_.snapshot())

  private def untilReachable[A](body: HttpControllerApi => Task[A]): Task[Checked[A]] = {
    // TODO Similar to SessionApi.retryUntilReachable
    val delays = SessionApi.defaultLoginDelays()
    var warned = now - 1.h
    /*HttpClient.liftProblem*/(
      apiResource
        .use(api => api
          .retryIfSessionLost()(body(api))
          .materialize.map {
            case Failure(t: HttpException) if t.statusInt == 503/*Service unavailable*/ =>
              Failure(t)  // Trigger onErrorRestartLoop
            case o => HttpClient.failureToChecked(o)
          }
          .dematerialize)
        .onErrorRestartLoop(()) {
          case (t, _, retry) if HttpClient.isTemporaryUnreachable(t) && delays.hasNext =>
            if (warned.elapsed >= 60.s) {
              logger.warn(t.toStringWithCauses)
              warned = now
            } else {
              logger.debug(t.toStringWithCauses)
            }
            apiCache.clear *>
              Task.sleep(delays.next()) *>
              retry(())
          case (t, _, _) =>
            Task.raiseError(t)
        })
  }
}

object ControllerApi
{
  private val logger = scribe.Logger[this.type]

  def resource(
    apiResources: Seq[Resource[Task, HttpControllerApi]],
    proxyConf: ProxyConf = ProxyConf.default)
  : Resource[Task, ControllerApi] =
    Resource.make(Task { new ControllerApi(apiResources, proxyConf) })(_.stop)
}
