package js7.proxy

import cats.effect.Resource
import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.{Json, JsonObject}
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.base.web.HttpClient
import js7.controller.client.HttpControllerApi
import js7.controller.data.ControllerCommand.{AddOrders, ReleaseEvents}
import js7.controller.data.{ControllerCommand, ControllerState}
import js7.data.event.{Event, EventId}
import js7.data.item.{UpdateRepoOperation, VersionId}
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
    apiResources.toVector.sequence.flatMap(apis =>
      Resource.liftF(
        JournaledProxy.selectActiveNodeApi(apis, onCouplingError = _.logError)))

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

  def updateRepo(versionId: VersionId, operations: Observable[UpdateRepoOperation.ItemOperation]): Task[Checked[Completed]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.postObservable[UpdateRepoOperation, JsonObject](
            "controller/api/repo",
            UpdateRepoOperation.AddVersion(versionId) +: operations
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
}
