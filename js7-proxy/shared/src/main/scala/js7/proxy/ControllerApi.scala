package js7.proxy

import cats.effect.Resource
import io.circe.{Json, JsonObject}
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.base.web.{HttpClient, Uri}
import js7.controller.client.HttpControllerApi
import js7.controller.data.ControllerCommand.Response.Accepted
import js7.controller.data.ControllerCommand.{AddOrders, ReleaseEvents}
import js7.controller.data.ControllerState.generic.{itemPathJsonCodec, simpleItemIdJsonCodec}
import js7.controller.data.ControllerState.simpleItemJsonCodec
import js7.controller.data.{ControllerCommand, ControllerState}
import js7.data.agent.AgentRef
import js7.data.cluster.ClusterSetting
import js7.data.event.{Event, EventId, JournalInfo}
import js7.data.item.{ItemOperation, SimpleItem, VersionId}
import js7.data.node.NodeId
import js7.data.order.FreshOrder
import js7.proxy.JournaledProxy.EndOfEventStreamException
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.ProxyEvent
import js7.proxy.data.event.EventAndState
import monix.eval.Task
import monix.reactive.Observable

final class ControllerApi(
  apiResources: Seq[Resource[Task, HttpControllerApi]],
  proxyConf: ProxyConf = ProxyConf.default)
extends ControllerApiWithHttp
{
  protected val apiResource: Resource[Task, HttpControllerApi] =
    JournaledProxy.selectActiveNodeApi(apiResources,
      onCouplingError = api => t => api.logError(t).void)

  /** For testing (it's slow): wait for a condition in the running event stream. **/
  def when(predicate: EventAndState[Event, ControllerState] => Boolean): Task[EventAndState[Event, ControllerState]] =
    JournaledProxy.observable(apiResources, None, _ => (), proxyConf)
      .filter(predicate)
      .headOptionL
      .map(_.getOrElse(throw new EndOfEventStreamException))

  /** Read events and state from Controller. */
  def eventAndStateObservable(proxyEventBus: StandardEventBus[ProxyEvent], eventId: Option[EventId])
  : Observable[EventAndState[Event, ControllerState]] =
    JournaledProxy.observable(apiResources, eventId, proxyEventBus.publish, proxyConf)

  def startProxy(
    proxyEventBus: StandardEventBus[ProxyEvent] = new StandardEventBus[ProxyEvent],
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
      .map(ItemOperation.SimpleAddOrReplace.apply))
      .map(_.map((_: Completed) => Accepted))

  @deprecated("Use updateItems", "2020-12-11")
  def updateRepo(versionId: VersionId, operations: Observable[ItemOperation.UpdateRepoOperation]): Task[Checked[Completed]] =
    updateItems(ItemOperation.AddVersion(versionId) +: operations)

  def updateSimpleItems(items: Seq[SimpleItem]): Task[Checked[Completed]] =
    updateItems(
      Observable.fromIterable(items) map ItemOperation.SimpleAddOrReplace.apply)

  def updateItems(operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.postObservable[ItemOperation, JsonObject](
            "controller/api/item",
            operations
          ).map(_ => Completed))))

  def addOrders(orders: Observable[FreshOrder]): Task[Checked[AddOrders.Response]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.postObservable[FreshOrder, Json]("controller/api/order", orders)
        ).map(_.flatMap(_.checkedAs[AddOrders.Response]))))

  /** @return true if added, otherwise false because of duplicate OrderId. */
  def addOrder(order: FreshOrder): Task[Checked[Boolean]] =
    executeCommand(ControllerCommand.AddOrder(order))
      .mapt(o => !o.ignoredBecauseDuplicate)

  def releaseEvents(eventId: EventId): Task[Checked[Completed]] =
    executeCommand(ReleaseEvents(eventId))
      .mapt((_: ControllerCommand.Response.Accepted) => Completed)

  def executeCommand(command: ControllerCommand): Task[Checked[command.Response]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.executeCommand(command))))

  def journalInfo: Task[Checked[JournalInfo]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        api.journalInfo))

  def controllerState: Task[Checked[ControllerState]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        api.snapshot()))
}
