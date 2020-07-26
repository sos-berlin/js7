package js7.proxy

import cats.effect.Resource
import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.controller.client.HttpControllerApi
import js7.controller.data.{ControllerCommand, ControllerState}
import js7.data.event.Event
import js7.data.order.FreshOrder
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

final class ControllerProxy private(
  protected val observable: Observable[EventAndState[Event, ControllerState]],
  protected val onEvent: EventAndState[Event, ControllerState] => Unit,
  apiResources: Seq[Resource[Task, HttpControllerApi]])
extends JournaledProxy[ControllerState]
with ControllerProxyWithHttp
{
  /** @return true iff added, false iff not added because of duplicate OrderId. */
  def addOrder(order: FreshOrder): Task[Checked[Boolean]] =
    execute(ControllerCommand.AddOrder(order))
      .map(_.map(o => !o.ignoredBecauseDuplicate))

  def executeCommand(command: ControllerCommand): Task[Checked[command.Response]] =
    execute(command)

  def execute(command: ControllerCommand): Task[Checked[command.Response]] =
    apiResource.use(api =>
      api.retryUntilReachable()(
        api.httpClient.liftProblem(
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
    onProxyEvent: ProxyEvent => Unit,
    onEvent: EventAndState[Event, ControllerState] => Unit,
    tornOlder: Option[FiniteDuration] = None)
  : ControllerProxy = {
    if (apiResources.isEmpty) throw new IllegalArgumentException("apiResources must not be empty")
    new ControllerProxy(
      JournaledProxy.observable(apiResources, onProxyEvent, tornOlder = tornOlder),
      onEvent,
      apiResources)
  }

  def start(
    apiResources: Seq[Resource[Task, HttpControllerApi]],
    onProxyEvent: ProxyEvent => Unit,
    onEvent: EventAndState[Event, ControllerState] => Unit,
    tornOlder: Option[FiniteDuration] = None)
  : Task[ControllerProxy] = {
    val proxy = apply(apiResources, onProxyEvent, onEvent, tornOlder)
    proxy.startObserving.map(_ => proxy)
  }
}
