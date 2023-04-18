package js7.proxy

import cats.effect.Resource
import js7.base.eventbus.StandardEventBus
import js7.base.problem.Checked
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.controller.client.HttpControllerApi
import js7.data.controller.ControllerCommand.AddOrders
import js7.data.controller.ControllerState
import js7.data.event.Event
import js7.data.order.FreshOrder
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.{EventAndState, ProxyEvent}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

final class ControllerProxy private(
  api: ControllerApi,
  protected val baseObservable: Observable[EventAndState[Event, ControllerState]],
  val proxyEventBus: StandardEventBus[ProxyEvent],
  val eventBus: JournaledStateEventBus[ControllerState],
  protected val proxyConf: ProxyConf)
  (protected implicit val scheduler: Scheduler)
extends JournaledProxy[ControllerState]
{
  protected def S = ControllerState
  protected val onEvent = eventBus.publish

  def addOrders(orders: Observable[FreshOrder]): Task[Checked[AddOrders.Response]] =
    api.addOrders(orders)
      .flatMapT(response =>
        sync(response.eventId)
          .map(_ => Right(response)))
}

object ControllerProxy
{
  private[proxy] def start(
    api: ControllerApi,
    apisResource: Resource[Task, Nel[HttpControllerApi]],
    proxyEventBus: StandardEventBus[ProxyEvent],
    eventBus: JournaledStateEventBus[ControllerState],
    proxyConf: ProxyConf = ProxyConf.default)
  : Task[ControllerProxy] =
    Task.deferAction { implicit s =>
      val proxy = new ControllerProxy(
        api,
        JournaledProxy.observable(apisResource, fromEventId = None, proxyEventBus.publish, proxyConf),
        proxyEventBus,
        eventBus,
        proxyConf)
      proxy.startObserving.map(_ => proxy)
    }
}
