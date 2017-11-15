package com.sos.jobscheduler.master

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.Uri
import akka.pattern.ask
import com.google.common.io.Closer
import com.google.inject.Stage.PRODUCTION
import com.google.inject.{Guice, Injector, Module}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkautils.CatchingActor
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime.{sleep, _}
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.order.{MasterOrderKeeper, ScheduledOrderGeneratorKeeper}
import com.sos.jobscheduler.master.web.MasterWebServer
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import java.nio.file.Files.{createDirectory, exists}
import java.time.{Duration, Instant}
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.Success

/**
 * JobScheduler Agent.
 *
 * Integration test in engine-tests, for example com.sos.jobscheduler.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
abstract class RunningMaster private(
  val webServer: MasterWebServer,
  val orderClient: OrderClient,
  val orderKeeper: ActorRef,
  val terminated: Future[Completed],
  closer: Closer,
  @TestOnly val injector: Injector)
extends AutoCloseable
{
  def executeCommand(command: MasterCommand): Future[command.MyResponse]

  val localUri: Uri = webServer.localUri
  val eventCollector = injector.instance[EventCollector]
  private implicit val executionContext = injector.instance[ExecutionContext]

  def close() = closer.close()
}

object RunningMaster {
  val StartedAt = Instant.now()
  val VersionString = BuildInfo.buildVersion
  private val logger = Logger(getClass)

  def run[A](configuration: MasterConfiguration, timeout: Option[Duration] = None)(body: RunningMaster ⇒ Unit): Unit = {
    val master = apply(configuration) await timeout
    body(master)
    master.terminated await timeout
  }

  def apply(configuration: MasterConfiguration): Future[RunningMaster] =
    apply(new MasterModule(configuration))

  def apply(module: Module): Future[RunningMaster] =
    apply(Guice.createInjector(PRODUCTION, module))

  def apply(injector: Injector): Future[RunningMaster] = {
    val masterConfiguration = injector.instance[MasterConfiguration]
    logger.info(s"Agent ${BuildInfo.buildVersion} config=${masterConfiguration.configDirectoryOption getOrElse ""} data=${masterConfiguration.dataDirectory}")

    masterConfiguration.stateDirectory match {
      case o if !exists(o) ⇒ createDirectory(o)
      case _ ⇒
    }

    implicit val executionContext = injector.instance[ExecutionContext]
    implicit val actorSystem = injector.instance[ActorSystem]
    val eventIdGenerator = injector.instance[EventIdGenerator]
    val eventCollector = injector.instance[EventCollector]
    val closer = injector.instance[Closer]
    val webServer = injector.instance[MasterWebServer]

    val (orderKeeper, actorStopped) = CatchingActor.actorOf[Completed](
      _ ⇒ Props {
        new MasterOrderKeeper(
          masterConfiguration,
          injector.instance[ScheduledOrderGeneratorKeeper])(
          injector.instance[TimerService],
          eventIdGenerator,
          eventCollector,
          injector.instance[StampedKeyedEventBus]) },
      onStopped = _ ⇒ Success(Completed))

    val orderClient = {
      val ec = executionContext
      new OrderClient {
        import masterConfiguration.akkaAskTimeout

        implicit def executionContext = ec

        def events[E <: Event](request: EventRequest[E]): Future[Stamped[TearableEventSeq[Seq, KeyedEvent[E]]]] =
          eventIdGenerator.stampTearableEventSeq(eventCollector.byPredicate(request, (_: KeyedEvent[E]) ⇒ true))

        def order(orderId: OrderId): Future[Option[Order[Order.State]]] =
          (orderKeeper ? MasterOrderKeeper.Command.GetOrder(orderId)).mapTo[Option[Order[Order.State]]]

        def orders: Future[Stamped[Seq[Order[Order.State]]]] =
          (orderKeeper ? MasterOrderKeeper.Command.GetOrders).mapTo[Stamped[Seq[Order[Order.State]]]]

        def orderCount =
          (orderKeeper ? MasterOrderKeeper.Command.GetOrderCount).mapTo[Int]
      }
    }

    webServer.setOrderClient(orderClient)
    val webServerReady = webServer.start()

    val terminated = actorStopped andThen { case _ ⇒
      blocking {
        logger.debug("Delaying close to let HTTP server respond open requests")
        sleep(500.ms)
      }
      closer.close()  // Close automatically after termination
    }
    for (_ ← webServerReady) yield {
      def execCmd(command: MasterCommand): Future[command.MyResponse] = {
        import masterConfiguration.akkaAskTimeout
        (orderKeeper ? command) map { _.asInstanceOf[command.MyResponse] }
      }
      val master = new RunningMaster(webServer, orderClient, orderKeeper, terminated, closer, injector) {
        def executeCommand(command: MasterCommand) = execCmd(command)
      }
      webServer.setExecuteCommand(command ⇒ master.executeCommand(command))
      master
    }
  }
}
