package com.sos.jobscheduler.provider

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.files.DirectoryReader
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.core.filebased.TypedSourceReader
import com.sos.jobscheduler.data.order.FreshOrder
import com.sos.jobscheduler.master.client.HttpMasterApi
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import com.sos.jobscheduler.provider.scheduledorder.{OrderScheduleGenerator, ScheduledOrderGenerator, ScheduledOrderGeneratorReader}
import java.time.ZoneId
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait OrderProvider extends HasCloser
{
  protected def conf: ProviderConfiguration
  protected def masterApi: HttpMasterApi

  private lazy val typedSourceReader = new TypedSourceReader(conf.orderGeneratorsDirectory,
    new ScheduledOrderGeneratorReader(ZoneId.systemDefault) :: Nil)

  private val orderScheduleGenerator = new OrderScheduleGenerator(addOrders, conf.config)

  protected def startAddingOrders()(implicit s: Scheduler) =
    orderScheduleGenerator.start()

  private def addOrders(orders: Seq[FreshOrder]): Task[Unit] =
    Task.sequence(orders map masterApi.addOrder)  // TODO API: addOrders Webservice akzeptiert Array[FreshOrder]
      .map((_: Seq[Boolean]) => ())

  onClose {
    orderScheduleGenerator.close()
  }

  protected final def replaceOrderGenerators: Checked[Unit] =
    typedSourceReader.readFileBaseds(DirectoryReader.entries(conf.orderGeneratorsDirectory).map(_.file))
      .map(_.map(_.asInstanceOf[ScheduledOrderGenerator]))
      .map(orderScheduleGenerator.replaceGenerators)
}
