package js7.provider

import java.time.ZoneId
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.utils.HasCloser
import js7.common.files.DirectoryReader
import js7.controller.client.HttpControllerApi
import js7.core.item.TypedSourceReader
import js7.data.order.FreshOrder
import js7.provider.configuration.ProviderConfiguration
import js7.provider.scheduledorder.{OrderScheduleGenerator, ScheduledOrderGenerator, ScheduledOrderGeneratorReader}
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.utils.ScalaUtils.syntax.RichF_

/**
  * @author Joacim Zschimmer
  */
trait OrderProvider extends HasCloser:

  protected def conf: ProviderConfiguration
  protected def httpControllerApi: HttpControllerApi
  protected def retryUntilNoError[A](body: => IO[Checked[A]]): IO[A]

  private lazy val typedSourceReader = new TypedSourceReader(conf.orderGeneratorsDirectory,
    new ScheduledOrderGeneratorReader(ZoneId.systemDefault) :: Nil)

  private val orderScheduleGenerator = new OrderScheduleGenerator(addOrders, conf.config)

  protected def startAddingOrders()(using IORuntime) =
    orderScheduleGenerator.start()

  private def addOrders(orders: Seq[FreshOrder]): IO[Completed] =
    retryUntilNoError {
      httpControllerApi.login(onlyIfNotLoggedIn = true) >>
        httpControllerApi
          .addOrders(orders.map(_.copy(deleteWhenTerminated = true)))
          .map(Right.apply)
    }.unless(orders.isEmpty)

  onClose:
    orderScheduleGenerator.close()

  protected final def replaceOrderGenerators: Checked[Unit] =
    typedSourceReader
      .readItems(DirectoryReader.entries(conf.orderGeneratorsDirectory).map(_.file))
      .map(_.map(_.asInstanceOf[ScheduledOrderGenerator]))
      .map(orderScheduleGenerator.replaceGenerators)
