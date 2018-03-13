package com.sos.jobscheduler.master

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.Uri
import akka.pattern.ask
import com.google.common.io.Closer
import com.google.inject.Stage.PRODUCTION
import com.google.inject.util.Modules.EMPTY_MODULE
import com.google.inject.{Guice, Injector, Module}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkautils.CatchingActor
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.event.{Event, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.order.MasterOrderKeeper
import com.sos.jobscheduler.master.web.MasterWebServer
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Path
import java.time.{Duration, Instant}
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

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
  (implicit ec: ExecutionContext)
extends AutoCloseable
{
  def executeCommand(command: MasterCommand): Future[command.MyResponse]

  def addOrder(order: Order[Order.Fresh]): Future[Boolean] =
    addOrder(FreshOrder.fromOrder(order))

  def addOrder(order: FreshOrder): Future[Boolean] =
    orderClient.addOrder(order)

  val localUri: Uri = webServer.localUri
  val eventCollector = injector.instance[EventCollector]

  def close() = closer.close()
}

object RunningMaster {
  val StartedAt = Instant.now()
  private val logger = Logger(getClass)

  def run[A](configuration: MasterConfiguration, timeout: Option[Duration] = None)(body: RunningMaster ⇒ Unit)(implicit ec: ExecutionContext): Unit = {
    autoClosing(apply(configuration) await timeout) { master ⇒
      for (t ← master.terminated.failed) logger.error(t.toStringWithCauses, t)
      body(master)
      master.terminated await timeout
    }
  }

  def runForTest(directory: Path)(body: RunningMaster ⇒ Unit)(implicit ec: ExecutionContext): Unit =
    autoClosing(startForTest(directory) await 99.s) { master ⇒
      try {
        body(master)
        master.executeCommand(MasterCommand.Terminate) await 99.s
        master.terminated await 99.s
      } catch { case NonFatal(t) if master.terminated.failed.isCompleted ⇒
        t.addSuppressed(master.terminated.failed.successValue)
        throw t
      }
    }

  def startForTest(directory: Path, module: Module = EMPTY_MODULE)(implicit ec: ExecutionContext): Future[RunningMaster] =
    RunningMaster(Guice.createInjector(
      new MasterModule(MasterConfiguration.forTest(configAndData = directory / "master")),
      module))

  def apply(configuration: MasterConfiguration)(implicit ec: ExecutionContext): Future[RunningMaster] =
    apply(new MasterModule(configuration))

  private def apply(module: Module)(implicit ec: ExecutionContext): Future[RunningMaster] =
    apply(Guice.createInjector(PRODUCTION, module))

  def apply(injector: Injector)(implicit ec: ExecutionContext): Future[RunningMaster] = {
    val masterConfiguration = injector.instance[MasterConfiguration]
    logger.info(s"config=${masterConfiguration.configDirectory} data=${masterConfiguration.dataDirectory}")

    masterConfiguration.stateDirectory match {
      case o if !exists(o) ⇒ createDirectory(o)
      case _ ⇒
    }

    val actorSystem = injector.instance[ActorSystem]
    val eventIdGenerator = injector.instance[EventIdGenerator]
    val eventCollector = injector.instance[EventCollector]
    val closer = injector.instance[Closer]
    val webServer = injector.instance[MasterWebServer]

    val (orderKeeper, actorStopped) = CatchingActor.actorOf[Completed](
        _ ⇒ Props {
          new MasterOrderKeeper(
            masterConfiguration)(
            injector.instance[TimerService],
            eventIdGenerator,
            eventCollector,
            injector.instance[StampedKeyedEventBus]) },
        onStopped = _ ⇒ Success(Completed)
      )(actorSystem)

    val workflowClient: WorkflowClient = new WorkflowClient {
      import masterConfiguration.akkaAskTimeout

      implicit def executionContext = ec

      def workflow(path: WorkflowPath): Future[Option[Workflow]] =
        (orderKeeper ? MasterOrderKeeper.Command.GetWorkflow(path)).
          mapTo[Option[Workflow]]

      def workflows: Future[Stamped[Seq[Workflow]]] =
        (orderKeeper ? MasterOrderKeeper.Command.GetWorkflows).
          mapTo[Stamped[Seq[Workflow]]]

      //def workflowPaths =  TODO optimize
      //(orderKeeper ? MasterOrderKeeper.Command.GetWorkflowPaths).
      // mapTo[Stamped[Seq[WorkflowPath]]]

      def workflowCount = (orderKeeper ? MasterOrderKeeper.Command.GetWorkflowCount).
        mapTo[Int]
    }

    val orderClient = new OrderClient {
      import masterConfiguration.akkaAskTimeout

      def executionContext = ec

      def addOrder(order: FreshOrder) =
        (orderKeeper ? MasterOrderKeeper.Command.AddOrder(order))
          .mapTo[MasterOrderKeeper.Response.AddOrderAccepted] map (_.created)

      def events[E <: Event](request: EventRequest[E]): Future[Stamped[TearableEventSeq[Seq, KeyedEvent[E]]]] =
        eventIdGenerator.stampTearableEventSeq(eventCollector.byPredicate(request, (_: KeyedEvent[E]) ⇒ true))

      def order(orderId: OrderId): Future[Option[Order[Order.State]]] =
        (orderKeeper ? MasterOrderKeeper.Command.GetOrder(orderId))
          .mapTo[Option[Order[Order.State]]]

      def orders: Future[Stamped[Seq[Order[Order.State]]]] =
        (orderKeeper ? MasterOrderKeeper.Command.GetOrders)
          .mapTo[Stamped[Seq[Order[Order.State]]]]

      def orderCount =
        (orderKeeper ? MasterOrderKeeper.Command.GetOrderCount)
          .mapTo[Int]
    }

    webServer.setClients(workflowClient, orderClient)
    val webServerReady = webServer.start()

    val terminated = actorStopped
      .andThen { case Failure(t) ⇒
        logger.error(t.toStringWithCauses, t)
      }
      .andThen { case _ ⇒
        blocking {
          logger.debug("Delaying close to let HTTP server respond open requests")
          sleep(500.ms)
        }
        closer.close()  // Close automatically after termination
      }
    for (_ ← webServerReady) yield {
      def execCmd(command: MasterCommand): Future[command.MyResponse] = {
        import masterConfiguration.akkaAskTimeout
        (command match {
          case MasterCommand.EmergencyStop ⇒
            val msg = "Command EmergencyStop received: JOBSCHEDULER MASTER STOPS NOW"
            logger.error(msg)
            Log4j.shutdown()
            sys.runtime.halt(99)
            Future.successful(MasterCommand.Response.Accepted)  // unreachable

          case _ ⇒
            orderKeeper ? command
        }) map (_.asInstanceOf[command.MyResponse])
      }
      val master = new RunningMaster(webServer, orderClient, orderKeeper, terminated, closer, injector) {
        def executeCommand(command: MasterCommand) = execCmd(command)
      }
      webServer.setExecuteCommand(command ⇒ master.executeCommand(command))
      master
    }
  }
}
