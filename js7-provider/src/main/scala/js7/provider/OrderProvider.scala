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
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait OrderProvider extends HasCloser
{
  protected def conf: ProviderConfiguration
  protected def controllerApi: HttpControllerApi
  protected def retryUntilNoError[A](body: => Task[Checked[A]]): Task[A]

  private lazy val typedSourceReader = new TypedSourceReader(conf.orderGeneratorsDirectory,
    new ScheduledOrderGeneratorReader(ZoneId.systemDefault) :: Nil)

  private val orderScheduleGenerator = new OrderScheduleGenerator(addOrders, conf.config)

  protected def startAddingOrders()(implicit s: Scheduler) =
    orderScheduleGenerator.start()

  private def addOrders(orders: Seq[FreshOrder]): Task[Completed] =
    retryUntilNoError {
      controllerApi.login(onlyIfNotLoggedIn = true) >>
        controllerApi.addOrders(orders)
          .flatMap((_: Completed) =>
            controllerApi.removeOrdersWhenTerminated(orders.map(_.id)))
          .map(Right.apply)
    }

  onClose {
    orderScheduleGenerator.close()
  }

  protected final def replaceOrderGenerators: Checked[Unit] =
    typedSourceReader.readInventoryItems(DirectoryReader.entries(conf.orderGeneratorsDirectory).map(_.file))
      .map(_.map(_.asInstanceOf[ScheduledOrderGenerator]))
      .map(orderScheduleGenerator.replaceGenerators)
}
