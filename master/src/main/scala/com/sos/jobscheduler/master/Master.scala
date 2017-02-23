package com.sos.scheduler.engine.master

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.scheduler.engine.base.generic.Completed
import com.sos.scheduler.engine.common.event.EventIdGenerator
import com.sos.scheduler.engine.common.event.collector.EventCollector
import com.sos.scheduler.engine.common.guice.GuiceImplicits.RichInjector
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.data.engine2.order.Order
import com.sos.scheduler.engine.data.event.{Event, EventRequest, EventSeq, KeyedEvent, Snapshot}
import com.sos.scheduler.engine.data.order.OrderId
import com.sos.scheduler.engine.master.Master._
import com.sos.scheduler.engine.master.command.MasterCommand
import com.sos.scheduler.engine.master.configuration.MasterConfiguration
import com.sos.scheduler.engine.master.configuration.inject.MasterModule
import com.sos.scheduler.engine.master.oldruntime.InstantInterval
import com.sos.scheduler.engine.master.order.{MasterOrderKeeper, ScheduledOrderGeneratorKeeper}
import com.sos.scheduler.engine.master.web.MasterWebServer
import com.sos.scheduler.engine.shared.event.SnapshotKeyedEventBus
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
