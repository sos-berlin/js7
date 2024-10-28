package js7.provider.scheduledorder

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.{scheduleOnce, unsafeToCancelableFuture}
import js7.base.monixlike.{SerialFutureCancelable, SyncCancelable}
import js7.base.time.JavaTimeConverters.*
import js7.base.time.JavaTimestamp.specific.*
import js7.base.time.Timestamp
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.FreshOrder
import js7.provider.scheduledorder.OrderScheduleGenerator.*
import js7.provider.scheduledorder.oldruntime.InstantInterval
import scala.annotation.nowarn
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class OrderScheduleGenerator(addOrders: Seq[FreshOrder] => IO[Completed], config: Config):
  private val addEvery = config.getDuration("js7.provider.add-orders-every").toFiniteDuration
  private val addEarlier = config.getDuration("js7.provider.add-orders-earlier").toFiniteDuration
  @volatile private var scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(Nil)
  @volatile private var generatedUntil = Timestamp.now.roundToNextSecond
  @volatile private var timer = SyncCancelable.empty
  private val started = Atomic(false)
  @volatile private var closed = false
  @nowarn("cat=deprecation")
  private val addOrdersCancelable = SerialFutureCancelable()

  def close(): Unit =
    closed = true
    timer.cancel()
    addOrdersCancelable.cancelAndForget()

  def replaceGenerators(generators: Seq[ScheduledOrderGenerator]): Unit =
    scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(generators)

  def start()(using IORuntime): Unit =
    if started.getAndSet(true) then throw new IllegalStateException(
      "OrderScheduleGenerator has already been started")
    generate()

  private def generate()(using ioRuntime: IORuntime): Unit =
    given ExecutionContext = ioRuntime.compute
    val interval = InstantInterval(generatedUntil.toInstant, addEvery.asJava)
    logger.debug(s"Generating orders for time interval $interval")
    val orders = scheduledOrderGeneratorKeeper.generateOrders(interval)
    if !closed then
      if orders.isEmpty then logger.debug("No orders generated in this time interval")
      val future = addOrders(orders).void.unsafeToCancelableFuture()
      addOrdersCancelable := future
      future.onComplete:
        case Success(()) =>
          continue()
        case Failure(t) =>
          logger.error(t.toStringWithCauses)
          logger.debug(t.toString, t)
          continue()

  private def continue()(using ioRuntime: IORuntime): Unit =
    generatedUntil += addEvery
    timer.cancel()
    timer = ioRuntime.scheduler.scheduleOnce(generatedUntil - addEarlier - Timestamp.now):
      generate()

  override def toString = "OrderScheduleGenerator"


object OrderScheduleGenerator:
  private val logger = Logger[this.type]
