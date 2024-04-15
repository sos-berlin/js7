package js7.proxy

import cats.effect.{IO, ResourceIO}
import fs2.Stream
import js7.base.eventbus.StandardEventBus
import js7.base.problem.Checked
import js7.base.service.Service
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.controller.client.HttpControllerApi
import js7.data.controller.ControllerCommand.AddOrders
import js7.data.controller.ControllerState
import js7.data.order.FreshOrder
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.ProxyEvent

final class ControllerProxy private[ControllerProxy](
  journaledProxy: JournaledProxy[ControllerState],
  val eventBus: JournaledStateEventBus[ControllerState],
  api: ControllerApi)
extends
  Service.StoppableByRequest, JournaledProxy[ControllerState]:

  export journaledProxy.*

  protected def start =
    startService(untilStopRequested)

  override def stop: IO[Unit] =
    super.stop

  def addOrders(orders: Stream[IO, FreshOrder]): IO[Checked[AddOrders.Response]] =
    api.addOrders(orders)
      .flatMapT: response =>
        journaledProxy.sync(response.eventId)
          .as(Right(response))


object ControllerProxy:

  private[proxy] def resource(
    api: ControllerApi,
    apisResource: ResourceIO[Nel[HttpControllerApi]],
    proxyEventBus: StandardEventBus[ProxyEvent],
    eventBus: JournaledStateEventBus[ControllerState],
    proxyConf: ProxyConf = ProxyConf.default)
  : ResourceIO[ControllerProxy] =
      for
        journaledProxy <- JournaledProxy.resource(
          JournaledProxy.stream(apisResource, fromEventId = None, proxyEventBus.publish, proxyConf),
          proxyConf,
          eventBus.publish)
        controllerProxy <- Service.resource(IO:
          new ControllerProxy(journaledProxy, eventBus, api))
      yield
        controllerProxy
