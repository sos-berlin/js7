package js7.proxy

import cats.effect.Resource
import js7.base.eventbus.StandardEventBus
import js7.controller.client.HttpControllerApi
import js7.controller.data.ControllerState
import js7.data.event.Event
import js7.proxy.configuration.ProxyConf
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.collection.immutable.Seq

final class ControllerProxy private(
  protected val baseObservable: Observable[EventAndState[Event, ControllerState]],
  val proxyEventBus: StandardEventBus[ProxyEvent],
  val eventBus: JournaledStateEventBus[ControllerState])
  (protected implicit val scheduler: Scheduler)
extends JournaledProxy[ControllerState]
{
  protected def S = ControllerState
  protected val onEvent = eventBus.publish
}

object ControllerProxy
{
  def start(
    apiResources: Seq[Resource[Task, HttpControllerApi]],
    proxyEventBus: StandardEventBus[ProxyEvent],
    eventBus: JournaledStateEventBus[ControllerState],
    proxyConf: ProxyConf = ProxyConf.default)
  : Task[ControllerProxy] =
    Task.deferAction { implicit s =>
      val proxy = new ControllerProxy(
        JournaledProxy.observable(apiResources, fromEventId = None, proxyEventBus.publish, proxyConf),
        proxyEventBus,
        eventBus)
      proxy.startObserving.map(_ => proxy)
    }
}
