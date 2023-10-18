package js7.provider.scheduledorder

import com.typesafe.config.Config
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.time.JavaTimeConverters.*
import js7.base.time.JavaTimestamp.specific.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.FreshOrder
import js7.provider.scheduledorder.OrderScheduleGenerator.*
import js7.provider.scheduledorder.oldruntime.InstantInterval
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, Scheduler}
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class OrderScheduleGenerator(addOrders: Seq[FreshOrder] => Task[Completed], config: Config):
  private val addEvery = config.getDuration("js7.provider.add-orders-every").toFiniteDuration
  private val addEarlier = config.getDuration("js7.provider.add-orders-earlier").toFiniteDuration
  @volatile private var scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(Nil)
  @volatile private var generatedUntil = Timestamp.now.roundToNextSecond
  @volatile private var timer = Cancelable.empty
  private val started = AtomicBoolean(false)
  @volatile private var closed = false
  private val addOrdersCancelable = SerialCancelable()

  def close() =
    closed = true
    timer.cancel()
    addOrdersCancelable.cancel()

  def replaceGenerators(generators: Seq[ScheduledOrderGenerator]): Unit =
    scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(generators)

  def start()(implicit scheduler: Scheduler): Unit =
    if started.getAndSet(true) then throw new IllegalStateException(
      "OrderScheduleGenerator has already been started")
    generate()

  private def generate()(implicit s: Scheduler): Unit =
    val interval = InstantInterval(generatedUntil.toInstant, addEvery.asJava)
    logger.debug(s"Generating orders for time interval $interval")
    val orders = scheduledOrderGeneratorKeeper.generateOrders(interval)
    if !closed then
      if orders.isEmpty then logger.debug("No orders generated in this time interval")
      val future = addOrders(orders).runToFuture
      addOrdersCancelable := future
      future onComplete:
        case Success(Completed) =>
          continue()
        case Failure(t) =>
          logger.error(t.toStringWithCauses)
          logger.debug(t.toString, t)
          continue()

  private def continue()(implicit scheduler: Scheduler): Unit =
    generatedUntil += addEvery
    timer.cancel()
    timer = scheduler.scheduleOnce(generatedUntil - addEarlier - Timestamp.now):
      generate()

  override def toString = "OrderScheduleGenerator"


object OrderScheduleGenerator:
  private val logger = Logger[this.type]
