package js7.provider

import java.time.ZoneId
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.utils.HasCloser
import js7.common.files.DirectoryReader
import js7.core.filebased.TypedSourceReader
import js7.data.order.FreshOrder
import js7.master.client.HttpMasterApi
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
  protected def masterApi: HttpMasterApi
  protected def retryUntilNoError[A](body: => Task[Checked[A]]): Task[A]

  private lazy val typedSourceReader = new TypedSourceReader(conf.orderGeneratorsDirectory,
    new ScheduledOrderGeneratorReader(ZoneId.systemDefault) :: Nil)

  private val orderScheduleGenerator = new OrderScheduleGenerator(addOrders, conf.config)

  protected def startAddingOrders()(implicit s: Scheduler) =
    orderScheduleGenerator.start()

  private def addOrders(orders: Seq[FreshOrder]): Task[Completed] =
    retryUntilNoError {
      masterApi.login(onlyIfNotLoggedIn = true) >>
        masterApi.addOrders(orders)
          .map(Right.apply)
    }

  onClose {
    orderScheduleGenerator.close()
  }

  protected final def replaceOrderGenerators: Checked[Unit] =
    typedSourceReader.readFileBaseds(DirectoryReader.entries(conf.orderGeneratorsDirectory).map(_.file))
      .map(_.map(_.asInstanceOf[ScheduledOrderGenerator]))
      .map(orderScheduleGenerator.replaceGenerators)
}
