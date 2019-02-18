package com.sos.jobscheduler.master.scheduledorder

import akka.Done
import akka.actor.{ActorRef, Stash, Status}
import cats.syntax.option._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.master.MasterOrderKeeper
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.oldruntime.InstantInterval
import com.sos.jobscheduler.master.scheduledorder.OrderScheduleGenerator._
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.Iterable
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class OrderScheduleGenerator(
  val journalActor: ActorRef,
  masterOrderKeeper: ActorRef,
  masterConfiguration: MasterConfiguration)
  (implicit scheduler: Scheduler)
extends KeyedJournalingActor[OrderScheduleEvent] with Stash {

  private var scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(masterConfiguration, Nil)
  private var generatedUntil: Timestamp = null
  private var recovered = false
  private var timer = none[Cancelable]

  def key = Key

  def snapshot = Option(generatedUntil) map OrderScheduleEndedAt.apply

  override def postStop(): Unit = {
    timer foreach (_.cancel())
    super.postStop()
  }

  protected def recoverFromSnapshot(snapshot: Any) = PartialFunction.empty

  protected def recoverFromEvent(event: OrderScheduleEvent) = PartialFunction.empty

  def receive = {
    case Input.Recover(until) ⇒
      generatedUntil = until
      recovered = true

    case Input.Change(scheduledOrderGenerators) ⇒
      scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(masterConfiguration, scheduledOrderGenerators)

    case Input.ScheduleEvery(every) ⇒
      val nw = Timestamp.ofEpochSecond(now.toEpochSecond)  // Last full second
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
      timer foreach (_.cancel())
      timer = Some(
        scheduler.scheduleOnce(generatedUntil - GenerateBeforeDuration - now) {
          self ! Internal.Generate(every)
        })

    case Internal.Generate(every) ⇒
      val interval = InstantInterval(generatedUntil.toInstant, every.toJavaDuration)
      logger.debug(s"Generating orders for time interval $interval")
      val orders = scheduledOrderGeneratorKeeper.generateOrders(interval)
      masterOrderKeeper ! MasterOrderKeeper.Command.AddOrderSchedule(orders)
      become("addingOrderSchedule")(addingOrderSchedule(interval.until.toTimestamp, every))
  }

  private def addingOrderSchedule(until: Timestamp, every: FiniteDuration): Receive = {
    case Done if sender() == masterOrderKeeper ⇒
      onOrdersAdded(until, every)
    case Status.Failure(t) if sender() == masterOrderKeeper ⇒
      logger.error(t.toString, t)
      onOrdersAdded(until, every)
    case _ ⇒
      stash()
  }

  private def onOrdersAdded(until: Timestamp, every: FiniteDuration): Unit = {
    become("receive")(receive)
    unstashAll()
    persist(OrderScheduleEvent.GeneratedUntil(until)) { e ⇒
      update(e)
      self ! Internal.ScheduleNextGeneration(every)
    }
  }

  private def update(event: OrderScheduleEvent): Unit = event match {
    case OrderScheduleEvent.GeneratedUntil(until) ⇒
      generatedUntil = until
  }

  override def toString = "OrderScheduleGenerator"
}

object OrderScheduleGenerator {
  val Key = NoKey
  private val GenerateBeforeDuration = 10.seconds
  private val logger = Logger(getClass)

  object Input {
    final case class Recover(generatedUntil: Timestamp)
    final case class Change(scheduledOrderGenerators: Iterable[ScheduledOrderGenerator])
    final case class ScheduleEvery(every: FiniteDuration)
  }

  private object Internal {
    final case class ScheduleNextGeneration(every: FiniteDuration)
    final case class Generate(every: FiniteDuration)
  }
}
