package js7.provider

import cats.effect.{IO, ResourceIO}
import java.time.ZoneId
import js7.base.problem.Checked
import js7.base.service.Service
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.files.DirectoryReader
import js7.controller.client.HttpControllerApi
import js7.core.item.TypedSourceReader
import js7.data.order.FreshOrder
import js7.provider.configuration.ProviderConfiguration
import js7.provider.scheduledorder.{OrderScheduleGenerator, ScheduledOrderGenerator, ScheduledOrderGeneratorReader}

final class OrderProvider private(
  orderScheduleGenerator: OrderScheduleGenerator,
  httpControllerApi: HttpControllerApi,
  conf: ProviderConfiguration)
extends
  Service.StoppableByRequest:

  private lazy val typedSourceReader = new TypedSourceReader(conf.orderGeneratorsDirectory,
    new ScheduledOrderGeneratorReader(ZoneId.systemDefault) :: Nil)

  protected def start =
    startService:
      untilStopRequested

  def replaceOrderGenerators: Checked[Unit] =
    typedSourceReader
      .readItems(DirectoryReader.entries(conf.orderGeneratorsDirectory).map(_.file))
      .map(_.map(_.asInstanceOf[ScheduledOrderGenerator]))
      .map(orderScheduleGenerator.replaceGenerators)


object OrderProvider:

  def service(
    httpControllerApi: HttpControllerApi,
    retryUntilNoError: IO[Checked[Unit]] => IO[Unit],
    conf: ProviderConfiguration)
  : ResourceIO[OrderProvider] =

    def addOrders(orders: Seq[FreshOrder]): IO[Unit] =
      retryUntilNoError:
        httpControllerApi.login(onlyIfNotLoggedIn = true) >>
          httpControllerApi
            .addOrders(orders.map(_.copy(deleteWhenTerminated = true)))
            .as(Right(()))
      .unless(orders.isEmpty)

    for
      orderScheduleGenerator <- OrderScheduleGenerator.service(addOrders, conf.config)
      service <- Service.resource:
        OrderProvider(orderScheduleGenerator, httpControllerApi, conf)
    yield
      service
