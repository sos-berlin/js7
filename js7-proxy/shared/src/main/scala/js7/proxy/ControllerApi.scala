package js7.proxy

import cats.effect.Resource
import io.circe.{Json, JsonObject}
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.Signed
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEitherF}
import js7.base.web.{HttpClient, Uri}
import js7.controller.client.HttpControllerApi
import js7.data.agent.AgentRef
import js7.data.cluster.ClusterSetting
import js7.data.controller.ControllerCommand.Response.Accepted
import js7.data.controller.ControllerCommand.{AddOrder, AddOrders, Batch, DeleteOrdersWhenTerminated, ReleaseEvents}
import js7.data.controller.ControllerState._
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.{Event, EventId, JournalInfo}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, RemoveVersioned}
import js7.data.item.{ItemOperation, ItemSigner, SignableItem, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath, VersionedItems}
import js7.data.node.NodeId
import js7.data.order.{FreshOrder, OrderId}
import js7.proxy.JournaledProxy.EndOfEventStreamException
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.{EventAndState, ProxyEvent}
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.immutable

final class ControllerApi(
  apiResources: Seq[Resource[Task, HttpControllerApi]],
  proxyConf: ProxyConf = ProxyConf.default)
extends ControllerApiWithHttp
with AutoCloseable
{
  protected val apiResource: Resource[Task, HttpControllerApi] =
    JournaledProxy.selectActiveNodeApi(apiResources,
      onCouplingError = api => t => api.logError(t).void)

  /** No operation, but this may change in future. */
  def close() = {}

  def stop: Task[Unit] =
    Task.unit

  /** For testing (it's slow): wait for a condition in the running event stream. **/
  def when(predicate: EventAndState[Event, ControllerState] => Boolean): Task[EventAndState[Event, ControllerState]] =
    JournaledProxy.observable(apiResources, None, _ => (), proxyConf)
      .filter(predicate)
      .headOptionL
      .map(_.getOrElse(throw new EndOfEventStreamException))

  /** Read events and state from Controller. */
  def eventAndStateObservable(
    proxyEventBus: StandardEventBus[ProxyEvent] = new StandardEventBus,
    eventId: Option[EventId] = None)
  : Observable[EventAndState[Event, ControllerState]] =
    JournaledProxy.observable(apiResources, eventId, proxyEventBus.publish, proxyConf)

  def startProxy(
    proxyEventBus: StandardEventBus[ProxyEvent] = new StandardEventBus,
    eventBus: JournaledStateEventBus[ControllerState] = new JournaledStateEventBus[ControllerState])
  : Task[ControllerProxy] =
    ControllerProxy.start(this, apiResources, proxyEventBus, eventBus, proxyConf)

  def clusterAppointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId, clusterWatches: Seq[ClusterSetting.Watch])
  : Task[Checked[Accepted]] =
    executeCommand(ControllerCommand.ClusterAppointNodes(idToUri, activeId, clusterWatches))

  @deprecated
  def updateAgentRefs(agentRefs: Seq[AgentRef]): Task[Checked[Accepted]] =
    updateItems(Observable
      .fromIterable(agentRefs)
      .map(ItemOperation.AddOrChangeSimple.apply))
      .map(_.map((_: Completed) => Accepted))

  def updateRepo(
    itemSigner: ItemSigner[VersionedItem],
    versionId: VersionId,
    diff: VersionedItems.Diff[VersionedItemPath, VersionedItem])
  : Task[Checked[Completed]] = {
    val addOrChange = Observable.fromIterable(diff.added ++ diff.changed)
      .map(_ withVersion versionId)
      .map(itemSigner.toSignedString)
      .map(AddOrChangeSigned.apply)
    val remove = Observable.fromIterable(diff.removed).map(RemoveVersioned.apply)
    updateItems(AddVersion(versionId) +: (addOrChange ++ remove))
  }

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
  def addOrder(order: FreshOrder, delete: Boolean = false): Task[Checked[Boolean]] =
    executeCommand(Batch(
      AddOrder(order) :: delete.thenList(DeleteOrdersWhenTerminated(Set(order.id)))))
      .map(_.flatMap(o => o
        .responses.head
        .map(response => !response.asInstanceOf[AddOrder.Response].ignoredBecauseDuplicate)))

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

  private def untilReachable[A](body: HttpControllerApi => Task[A]): Task[Checked[A]] =
    HttpClient.liftProblem(
      apiResource.use(api =>
        api.retryUntilReachable()(
          body(api))))
}

object ControllerApi
{
  def resource(
    apiResources: Seq[Resource[Task, HttpControllerApi]],
    proxyConf: ProxyConf = ProxyConf.default)
  : Resource[Task, ControllerApi] =
    Resource.make(Task { new ControllerApi(apiResources, proxyConf) })(_.stop)
}
