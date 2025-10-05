package js7.provider.scheduledorder

import cats.effect.{IO, ResourceIO}
import com.typesafe.config.Config
import js7.base.fs2utils.StreamExtensions.+:
import js7.base.log.Logger
import js7.base.service.Service
import js7.base.time.JavaTimeConverters.*
import js7.base.time.JavaTimestamp.specific.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.FreshOrder
import js7.provider.scheduledorder.OrderScheduleGenerator.*
import js7.provider.scheduledorder.oldruntime.InstantInterval

final class OrderScheduleGenerator private(addOrders: Seq[FreshOrder] => IO[Unit], config: Config)
  extends Service.StoppableByRequest:

  private val addEvery = config.getDuration("js7.provider.add-orders-every").toFiniteDuration
  private val addEarlier = config.getDuration("js7.provider.add-orders-earlier").toFiniteDuration
  @volatile private var scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(Nil)

  protected def start =
    startService:
      generate.background.surround:
        untilStopRequested

  def replaceGenerators(generators: Seq[ScheduledOrderGenerator]): Unit =
    scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(generators)

  private def generate: IO[Unit] =
    IO.defer:
      val start = Timestamp.now
      (0.s +: fs2.Stream.awakeEvery[IO](addEvery)).evalMap: diff =>
        val ts = start + diff
        val interval = InstantInterval(ts.toInstant, addEvery.asJava)
        logger.debug(s"Generating orders for time interval $interval")
        val orders = scheduledOrderGeneratorKeeper.generateOrders(interval)
        if orders.isEmpty then logger.debug("No orders generated in this time interval")
        addOrders(orders)
          .handleError: t =>
            logger.error(t.toStringWithCauses)
            logger.debug(t.toString, t)
      .compile.drain

  override def toString = "OrderScheduleGenerator"


object OrderScheduleGenerator:
  private val logger = Logger[this.type]

  def service(addOrders: Seq[FreshOrder] => IO[Unit], config: Config)
  : ResourceIO[OrderScheduleGenerator] =
    Service.resource:
      OrderScheduleGenerator(addOrders, config)
