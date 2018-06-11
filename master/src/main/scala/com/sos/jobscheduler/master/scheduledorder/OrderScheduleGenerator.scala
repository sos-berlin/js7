package com.sos.jobscheduler.master.scheduledorder

import akka.Done
import akka.actor.{ActorRef, Stash, Status}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.master.MasterOrderKeeper
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.oldruntime.InstantInterval
import com.sos.jobscheduler.master.scheduledorder.OrderScheduleGenerator._
import java.time.Instant.now
import java.time.{Duration, Instant}
import monix.execution.Scheduler
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
final class OrderScheduleGenerator(
  val journalActor: ActorRef,
  masterOrderKeeper: ActorRef,
  masterConfiguration: MasterConfiguration)
  (implicit
    timerService: TimerService,
    scheduler: Scheduler)
extends KeyedJournalingActor[OrderScheduleEvent] with Stash {

  private var scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(masterConfiguration, Nil)
  private var versionId: VersionId = null
  private var generatedUntil: Instant = null
  private var recovered = false
  private var timer: Timer[Unit] = Timer.empty

  def key = Key

  def snapshot = Option(generatedUntil) map (o ⇒ OrderScheduleEndedAt(Timestamp.ofInstant(o)))

  override def postStop(): Unit = {
    timerService.cancel(timer)
    super.postStop()
  }

  protected def recoverFromSnapshot(snapshot: Any) = PartialFunction.empty

  protected def recoverFromEvent(event: OrderScheduleEvent) = PartialFunction.empty

  def receive = journaling orElse {
    case Input.Recover(until) ⇒
      generatedUntil = until.toInstant
      recovered = true

    case Input.Change(scheduledOrderGenerators) ⇒
      scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(masterConfiguration, scheduledOrderGenerators)

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
    final case class Recover(generatedUntil: Timestamp)
    final case class Change(scheduledOrderGenerators: Iterable[ScheduledOrderGenerator])
    final case class ScheduleEvery(every: Duration)
  }

  private object Internal {
    final case class ScheduleNextGeneration(every: Duration)
    final case class Generate(every: Duration)
  }
}
