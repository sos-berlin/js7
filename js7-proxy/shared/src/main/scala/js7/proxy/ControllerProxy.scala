package js7.proxy

import cats.effect.Resource
import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.JsonObject
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.web.HttpClient
import js7.controller.client.HttpControllerApi
import js7.controller.data.{ControllerCommand, ControllerState}
import js7.data.event.Event
import js7.data.filebased.{UpdateRepoOperation, VersionId}
import js7.data.order.FreshOrder
import js7.proxy.configuration.ProxyConf
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.immutable.Seq

final class ControllerProxy private(
  protected val observable: Observable[EventAndState[Event, ControllerState]],
  protected val onEvent: EventAndState[Event, ControllerState] => Unit,
  apiResources: Seq[Resource[Task, HttpControllerApi]])
extends JournaledProxy[ControllerState]
with ControllerProxyWithHttp
{
  def updateRepo(versionId: VersionId, operations: Observable[UpdateRepoOperation.ItemOperation]): Task[Checked[Completed]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.postObservable[UpdateRepoOperation, JsonObject](
            "controller/api/repo",
            UpdateRepoOperation.AddVersion(versionId) +: operations)
            .map(_ => Completed))))

  def addOrders(orders: Observable[FreshOrder]): Task[Checked[Completed]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.postObservable[FreshOrder, JsonObject]("controller/api/order", orders)
            .map(_ => Completed))))

  /** @return true iff added, false iff not added because of duplicate OrderId. */
  def addOrder(order: FreshOrder): Task[Checked[Boolean]] =
    execute(ControllerCommand.AddOrder(order))
      .map(_.map(o => !o.ignoredBecauseDuplicate))

  def executeCommand(command: ControllerCommand): Task[Checked[command.Response]] =
    execute(command)

  def execute(command: ControllerCommand): Task[Checked[command.Response]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        HttpClient.liftProblem(
          api.executeCommand(command))))

  def apiResource: Resource[Task, HttpControllerApi] =
    apiResources.toVector.sequence.flatMap(apis =>
      Resource.liftF(
        JournaledProxy.selectActiveNodeApi(apis, onCouplingError = _.logError)))
}

object ControllerProxy
{
  def apply(
    apiResources: Seq[Resource[Task, HttpControllerApi]],
    onProxyEvent: ProxyEvent => Unit = _ => (),
    onEvent: EventAndState[Event, ControllerState] => Unit = _ => (),
    proxyConf: ProxyConf = ProxyConf.default)
  : ControllerProxy = {
    if (apiResources.isEmpty) throw new IllegalArgumentException("apiResources must not be empty")
    new ControllerProxy(
      JournaledProxy.observable(apiResources, onProxyEvent, proxyConf),
      onEvent,
      apiResources)
  }

  def start(
    apiResources: Seq[Resource[Task, HttpControllerApi]],
    onProxyEvent: ProxyEvent => Unit,
    onEvent: EventAndState[Event, ControllerState] => Unit,
    proxyConf: ProxyConf = ProxyConf.default)
  : Task[ControllerProxy] = {
    val proxy = apply(apiResources, onProxyEvent, onEvent, proxyConf)
    proxy.startObserving.map(_ => proxy)
  }
}
