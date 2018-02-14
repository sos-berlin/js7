package com.sos.jobscheduler.master.order

import akka.Done
import akka.actor.{ActorRef, Stash, Status}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.master.oldruntime.InstantInterval
import com.sos.jobscheduler.master.order.OrderScheduleGenerator._
import java.time.Instant.now
import java.time.{Duration, Instant}

/**
  * @author Joacim Zschimmer
  */
final class OrderScheduleGenerator(
  val journalActor: ActorRef,
  masterOrderKeeper: ActorRef,
  scheduledOrderGeneratorKeeper: ScheduledOrderGeneratorKeeper)
  (implicit timerService: TimerService)
extends KeyedJournalingActor[OrderScheduleEvent] with Stash {

  import context.dispatcher

  private var generatedUntil: Instant = null
  private var recovered = false
  private var timer: Timer[Unit] = Timer.empty

  def key = Key

  def snapshot = Option(generatedUntil) map OrderScheduleEndedAt.apply

  override def postStop(): Unit = {
    timerService.cancel(timer)
    super.postStop()
  }

  protected def recoverFromSnapshot(snapshot: Any) = snapshot match {
    case OrderScheduleEndedAt(instant) ⇒
      generatedUntil = instant
      recovered = true
  }

  protected def recoverFromEvent(event: OrderScheduleEvent) = event match {
    case OrderScheduleEvent.GeneratedUntil(until) ⇒
      generatedUntil = until.toInstant
      recovered = true
  }

  def receive = journaling orElse {
    case Input.ScheduleEvery(every) ⇒
      val nw = Instant.ofEpochSecond(now.getEpochSecond)  // Last full second
      if (generatedUntil == null) {
        generatedUntil = nw
      } else
      if (recovered) {
        logger.info(s"Recovered order generation interval ends at $generatedUntil")
        generatedUntil = generatedUntil max nw
        recovered = false
      }
      self ! Internal.ScheduleNextGeneration(every)

    case Internal.ScheduleNextGeneration(every) ⇒
      timerService.cancel(timer)
      timer = timerService.at(generatedUntil - GenerateBeforeDuration, getClass.getSimpleName)
      timer onElapsed { self ! Internal.Generate(every) }

    case Internal.Generate(every) ⇒
      val interval = InstantInterval(generatedUntil, every)
      logger.info(s"Generating orders for time interval $interval")
      val orders = scheduledOrderGeneratorKeeper.generateOrders(interval)
      masterOrderKeeper ! MasterOrderKeeper.Command.AddOrderSchedule(orders)
      context.become(addingOrderSchedule(interval.until, every))
  }

  private def addingOrderSchedule(until: Instant, every: Duration): Receive = journaling orElse {
    case Done if sender() == masterOrderKeeper ⇒
      onOrdersAdded(until, every)
    case Status.Failure(t) if sender() == masterOrderKeeper ⇒
      logger.error(t.toString, t)
      onOrdersAdded(until, every)
    case _ ⇒
      stash()
  }

  private def onOrdersAdded(until: Instant, every: Duration): Unit = {
    context.become(receive)
    unstashAll()
    persist(OrderScheduleEvent.GeneratedUntil(until.toTimestamp)) { e ⇒
      update(e)
      self ! Internal.ScheduleNextGeneration(every)
    }
  }

  private def update(event: OrderScheduleEvent): Unit = event match {
    case OrderScheduleEvent.GeneratedUntil(until) ⇒
      generatedUntil = until.toInstant
  }

  override def toString = "OrderScheduleGenerator"
}

object OrderScheduleGenerator {
  val Key = NoKey
  private val GenerateBeforeDuration = 10.s
  private val logger = Logger(getClass)

  object Input {
    final case class ScheduleEvery(every: Duration)
  }

  private object Internal {
    final case class ScheduleNextGeneration(every: Duration)
    final case class Generate(every: Duration)
  }
}
