package js7.provider.scheduledorder

import js7.base.generic.Completed
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.RichThrowable
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
import js7.data.order.FreshOrder
import js7.provider.scheduledorder.OrderScheduleGenerator._
import js7.provider.scheduledorder.oldruntime.InstantInterval
import com.typesafe.config.Config
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.{Cancelable, Scheduler}
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class OrderScheduleGenerator(addOrders: Seq[FreshOrder] => Task[Completed], config: Config)
{
  private val addEvery = config.getDuration("js7.provider.add-orders-every").toFiniteDuration
  private val addEarlier = config.getDuration("js7.provider.add-orders-earlier").toFiniteDuration
  @volatile private var scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(Nil)
  @volatile private var generatedUntil = Timestamp.now.roundToNextSecond
  @volatile private var timer: Cancelable = Cancelable.empty
  private val started = AtomicBoolean(false)
  @volatile private var closed = false

  def close() = {
    closed = true
    timer.cancel()
  }

  def replaceGenerators(generators: Seq[ScheduledOrderGenerator]): Unit =
    scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(generators)

  def start()(implicit scheduler: Scheduler): Unit = {
    if (started.getAndSet(true)) throw new IllegalStateException("OrderScheduleGenerator has already been started")
    generate()
  }

  private def generate()(implicit s: Scheduler): Unit = {
    val interval = InstantInterval(generatedUntil.toInstant, addEvery.asJava)
    logger.debug(s"Generating orders for time interval $interval")
    val orders = scheduledOrderGeneratorKeeper.generateOrders(interval)
    if (!closed) {
      addOrders(orders).runToFuture onComplete {
        case Success(Completed) =>
          continue()
        case Failure(t) =>
          logger.error(t.toStringWithCauses)
          logger.debug(t.toString, t)
          continue()
      }
    }
  }

  private def continue()(implicit scheduler: Scheduler): Unit = {
    generatedUntil += addEvery
    timer.cancel()
    timer = scheduler.scheduleOnce(generatedUntil - addEarlier - Timestamp.now) {
      generate()
    }
  }

  override def toString = "OrderScheduleGenerator"
}

object OrderScheduleGenerator {
  private val logger = Logger(getClass)
}
