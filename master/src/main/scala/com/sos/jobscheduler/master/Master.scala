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
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.order.{MasterOrderKeeper, ScheduledOrderGeneratorKeeper}
import com.sos.jobscheduler.master.web.MasterWebServer
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import java.nio.file.Files
import javax.inject.{Inject, Singleton}
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}
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
  keyedEventBus: StampedKeyedEventBus,
  timerService: TimerService,
  actorSystem: ActorSystem,
  executionContext: ExecutionContext,
  closer: Closer) {

  import configuration.akkaAskTimeout

  for (o ← configuration.stateDirectoryOption if !Files.exists(o)) {
    Files.createDirectory(o)
  }

  private[master] val orderKeeper = actorSystem.actorOf(
    Props { new MasterOrderKeeper(configuration, scheduledOrderGeneratorKeeper)(timerService, eventIdGenerator, eventCollector, keyedEventBus) },
    "MasterOrderKeeper")

  def start(): Future[Completed] = {
    closer.registerAutoCloseable(webServer)
    (webServer.start(): Future[Unit]) map { _ ⇒ Completed }
  }

  def executeCommand(command: MasterCommand): Future[command.MyResponse] =
    (orderKeeper ? command) map { _.asInstanceOf[command.MyResponse] }

  def events[E <: Event](request: EventRequest[E]): Future[Stamped[TearableEventSeq[Seq, KeyedEvent[E]]]] =
    eventIdGenerator.stampTearableEventSeq(eventCollector.byPredicate(request, (_: KeyedEvent[E]) ⇒ true))

  def order(orderId: OrderId): Future[Option[Order[Order.State]]] =
    (orderKeeper ? MasterOrderKeeper.Command.GetOrder(orderId)).mapTo[Option[Order[Order.State]]]

  def orders: Future[Seq[Order[Order.State]]] =
    (orderKeeper ? MasterOrderKeeper.Command.GetOrders).mapTo[Seq[Order[Order.State]]]

  def localUri: Uri =
    webServer.localUri
}

object Master {
  def apply(masterConfiguration: MasterConfiguration): Master =
    Guice.createInjector(new MasterModule(masterConfiguration)).instance[Master]
}
