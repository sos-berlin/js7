package com.sos.jobscheduler.master

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.engine2.order.Order
import com.sos.jobscheduler.data.event.{Event, EventRequest, EventSeq, KeyedEvent, Snapshot}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.Master._
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.oldruntime.InstantInterval
import com.sos.jobscheduler.master.order.{MasterOrderKeeper, ScheduledOrderGeneratorKeeper}
import com.sos.jobscheduler.master.web.MasterWebServer
import com.sos.jobscheduler.shared.event.SnapshotKeyedEventBus
import java.nio.file.Files
import javax.inject.{Inject, Singleton}
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Failure
import spray.http.Uri

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class Master @Inject private
(implicit
  configuration: MasterConfiguration,
  webServer: MasterWebServer,
  scheduledOrderGeneratorKeeper: ScheduledOrderGeneratorKeeper,
  val eventCollector: EventCollector,
  eventIdGenerator: EventIdGenerator,
  keyedEventBus: SnapshotKeyedEventBus,
  timerService: TimerService,
  actorSystem: ActorSystem,
  executionContext: ExecutionContext,
  closer: Closer) {

  import configuration.akkaAskTimeout

  for (o ← configuration.stateDirectoryOption if !Files.exists(o)) {
    Files.createDirectory(o)
  }

  private[master] val orderKeeper = actorSystem.actorOf(
    Props { new MasterOrderKeeper(configuration)(timerService, eventIdGenerator, eventCollector, keyedEventBus) },
    "MasterOrderKeeper")

  def start(): Future[Completed] = {
    closer.registerAutoCloseable(webServer)
    (webServer.start(): Future[Unit]) map { _ ⇒ Completed }
  }

  def addScheduledOrders(instantInterval: InstantInterval): Seq[Order[Order.Idle]] = {
    val orders = generateOrders(instantInterval)
    for (order ← orders) {
      executeCommand(MasterCommand.AddOrderIfNew(order)) onComplete {
        case Failure(t) ⇒ logger.warn(s"AddOrderIfNew '${order.id}': $t")
        case _ ⇒
      }
    }
    orders
  }

  def generateOrders(instantInterval: InstantInterval): Seq[Order[Order.Scheduled]] =
    scheduledOrderGeneratorKeeper.generateOrders(instantInterval)

  def executeCommand(addOrder: MasterCommand.AddOrderIfNew): Future[addOrder.MyResponse] =
    (orderKeeper ? addOrder).mapTo[addOrder.MyResponse]

  def events[E <: Event](request: EventRequest[E]): Future[Snapshot[EventSeq[Seq, KeyedEvent[E]]]] =
    eventIdGenerator.wrapInSnapshot(eventCollector.byPredicate(request, (_: KeyedEvent[E]) ⇒ true))

  def getOrder(orderId: OrderId): Future[Option[Order[Order.State]]] =
    (orderKeeper ? MasterOrderKeeper.Command.Get(orderId)).mapTo[Option[Order[Order.State]]]

  def localUri: Uri =
    webServer.localUri
}

object Master {
  private val logger = Logger(getClass)
  def apply(masterConfiguration: MasterConfiguration): Master =
    Guice.createInjector(new MasterModule(masterConfiguration)).instance[Master]
}
