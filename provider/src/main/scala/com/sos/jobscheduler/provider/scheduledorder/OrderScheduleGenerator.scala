package com.sos.jobscheduler.provider.scheduledorder

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.data.order.FreshOrder
import com.sos.jobscheduler.provider.scheduledorder.OrderScheduleGenerator._
import com.sos.jobscheduler.provider.scheduledorder.oldruntime.InstantInterval
import com.typesafe.config.Config
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.Seq
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class OrderScheduleGenerator(addOrders: Seq[FreshOrder] => Task[Completed], config: Config)
{
  private val addEvery = config.getDuration("jobscheduler.provider.add-orders-every").toFiniteDuration
  private val addEarlier = config.getDuration("jobscheduler.provider.add-orders-earlier").toFiniteDuration
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
