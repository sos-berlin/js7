package com.sos.jobscheduler.master

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.Uri
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.io.Closer
import com.google.inject.Stage.{DEVELOPMENT, PRODUCTION}
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import com.google.inject.{Guice, Injector, Module}
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichPartialFunction, RichThrowable}
import com.sos.jobscheduler.common.akkahttp.web.session.{LoginSession, SessionRegister}
import com.sos.jobscheduler.common.akkautils.CatchingActor
import com.sos.jobscheduler.common.event.{EventIdClock, StrictEventReader}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.monix.MonixForCats._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.core.StartUp
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.{EventReaderProvider, JournalEventReaderProvider}
import com.sos.jobscheduler.core.filebased.{FileBasedApi, Repo}
import com.sos.jobscheduler.data.event.{Event, Stamped}
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, FileBasedsOverview, TypedPath}
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId}
import com.sos.jobscheduler.master.command.{CommandExecutor, CommandMeta}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.master.web.MasterWebServer
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Path
import java.time.Duration
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.{Future, blocking}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
 * JobScheduler Agent.
 *
 * Integration test in engine-tests, for example com.sos.jobscheduler.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class RunningMaster private(
  val sessionRegister: SessionRegister[LoginSession.Simple],
  val commandExecutor: CommandExecutor,
  val webServer: MasterWebServer,
  val orderApi: OrderApi.WithCommands,
  val orderKeeper: ActorRef,
  val terminated: Future[Completed],
  closer: Closer,
  @TestOnly val injector: Injector)
extends AutoCloseable
{
  def executeCommandAsSystemUser(command: MasterCommand): Task[Checked[command.MyResponse]] =
    for {
      checkedSession ← sessionRegister.systemSession
      checkedChecked ← checkedSession.map(session ⇒ executeCommand(command, CommandMeta(session.user))).evert
    } yield checkedChecked.flatten

  def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[command.MyResponse]] =
    commandExecutor.executeCommand(command, meta)

  def addOrder(order: FreshOrder): Task[Checked[Boolean]] =
    orderApi.addOrder(order)

  def addOrderBlocking(order: FreshOrder)(implicit s: Scheduler): Boolean =
    orderApi.addOrder(order).runAsync.await(99.s).orThrow

  val localUri: Uri = webServer.localUri
  val eventReader: StrictEventReader[Event] = injector.instance[EventReaderProvider[Event]].strict

  def close() = closer.close()
}

object RunningMaster {
  val StartedAt = Timestamp.now
  private val logger = Logger(getClass)

  def run[A](configuration: MasterConfiguration, timeout: Option[Duration] = None)(body: RunningMaster ⇒ Unit)(implicit s: Scheduler): Unit =
    autoClosing(apply(configuration) await timeout) { master ⇒
      for (t ← master.terminated.failed) logger.error(t.toStringWithCauses, t)
      body(master)
      master.terminated await timeout
    }

  def runForTest(directory: Path, eventCollector: Option[TestEventCollector] = None)(body: RunningMaster ⇒ Unit)(implicit s: Scheduler): Unit = {
    val injector = newInjectorForTest(directory)
    eventCollector foreach (_.start(injector.instance[ActorSystem], injector.instance[StampedKeyedEventBus]))
    runForTest(injector)(body)
  }

  def runForTest(injector: Injector)(body: RunningMaster ⇒ Unit)(implicit s: Scheduler): Unit =
    autoClosing(RunningMaster(injector) await 99.s) { master ⇒
      try {
        body(master)
        master.executeCommandAsSystemUser(MasterCommand.Terminate) await 99.s
        master.terminated await 99.s
      } catch { case NonFatal(t) if master.terminated.failed.isCompleted ⇒
        t.addSuppressed(master.terminated.failed.successValue)
        throw t
      }
    }

  def newInjectorForTest(directory: Path, module: Module = EMPTY_MODULE,
    httpPort: Option[Int] = Some(findRandomFreeTcpPort()), httpsPort: Option[Int] = None
  ): Injector =
    Guice.createInjector(DEVELOPMENT,
      Modules `override` new MasterModule(MasterConfiguration.forTest(
        configAndData = directory,
        httpPort = httpPort,
        httpsPort = httpsPort))
      `with` module)

  def apply(configuration: MasterConfiguration): Future[RunningMaster] =
    apply(new MasterModule(configuration))

  private def apply(module: Module): Future[RunningMaster] =
    apply(Guice.createInjector(PRODUCTION, module))

  def apply(injector: Injector): Future[RunningMaster] =
    new Starter(injector).start()

  private class Starter(injector: Injector) {
    private val closer = injector.instance[Closer]
    private val masterConfiguration = injector.instance[MasterConfiguration]
    private val actorSystem = injector.instance[ActorSystem]
    implicit private val scheduler = injector.instance[Scheduler]

    private def createDirectories(): Unit =
      masterConfiguration.stateDirectory match {
        case o if !exists(o) ⇒ createDirectory(o)
        case _ ⇒
      }

    private def createSessionTokenFile(sessionRegister: SessionRegister[LoginSession.Simple]): Unit = {
      val sessionTokenFile = masterConfiguration.stateDirectory / "session-token"
      sessionRegister.createSystemSession(SimpleUser.System, sessionTokenFile)
        .runAsync await masterConfiguration.akkaAskTimeout.duration
      closer onClose { sessionTokenFile.delete() }
    }

    private def startMasterOrderKeeper(): (ActorRef, Future[Completed]) =
      CatchingActor.actorOf[Completed](
          _ ⇒ Props {
            new MasterOrderKeeper(
              masterConfiguration,
              injector.instance[EventIdClock])(
              injector.instance[TimerService],
              injector.instance[JournalEventReaderProvider[Event]],
              injector.instance[StampedKeyedEventBus],
              scheduler)
          },
          onStopped = _ ⇒ Success(Completed)
        )(actorSystem)

    private[RunningMaster] def start(): Future[RunningMaster] = {
      StartUp.logStartUp(masterConfiguration.configDirectory, masterConfiguration.dataDirectory)
      createDirectories()

      val sessionRegister = injector.instance[SessionRegister[LoginSession.Simple]]
      createSessionTokenFile(sessionRegister)

      val (orderKeeper, orderKeeperStopped) = startMasterOrderKeeper()
      val fileBasedApi = new MainFileBasedApi(masterConfiguration, orderKeeper)
      val orderApi = new MainOrderApi(orderKeeper, masterConfiguration.akkaAskTimeout)
      val commandExecutor = new CommandExecutor(masterConfiguration, sessionRegister, orderKeeper = orderKeeper)

      val terminated = orderKeeperStopped
        .andThen { case Failure(t) ⇒ logger.error(t.toStringWithCauses, t) }
        .andThen { case _ ⇒
          blocking {
            logger.debug("Delaying to let HTTP server respond open requests")
            sleep(500.ms)
          }
          closer.close()  // Close automatically after termination
        }

      val webServer = injector.instance[MasterWebServer.Factory].apply(fileBasedApi, orderApi, commandExecutor)
      for (_ ← webServer.start()) yield
        new RunningMaster(sessionRegister, commandExecutor, webServer, orderApi, orderKeeper, terminated, closer, injector)
    }
  }

  private class MainFileBasedApi(masterConfiguration: MasterConfiguration, orderKeeper: ActorRef) extends FileBasedApi
  {
    def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Stamped[O.Overview]] =
      for (stamped ← getRepo) yield
        for (repo ← stamped) yield
          O.fileBasedsToOverview(repo.currentTyped[A].values.toImmutableSeq)

    def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
      for (stamped ← getRepo) yield
        for (repo ← stamped) yield
          repo.idTo[A](id)

    def fileBaseds[A <: FileBased: FileBased.Companion]: Task[Stamped[Seq[A]]] =
      for (stamped ← getRepo) yield
        for (repo ← stamped) yield
          repo.currentTyped[A].values.toImmutableSeq.sortBy/*for determinstic tests*/(_.id: FileBasedId[TypedPath])

    def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path): Task[Checked[Stamped[A]]] =
      for (stamped ← getRepo; repo = stamped.value) yield
        for (a ← repo.currentTyped[A].checked(path)) yield
          stamped.copy(value = a)

    private def getRepo: Task[Stamped[Repo]] = {
      import masterConfiguration.akkaAskTimeout  // TODO May timeout while Master recovers
      Task.deferFuture(
        (orderKeeper ? MasterOrderKeeper.Command.GetRepo).mapTo[Stamped[Repo]])
    }
  }

  private class MainOrderApi(orderKeeper: ActorRef, implicit private val akkaAskTimeout: Timeout) extends OrderApi.WithCommands
  {
    def addOrder(order: FreshOrder) =
      Task.deferFuture(
        (orderKeeper ? MasterOrderKeeper.Command.AddOrder(order)).mapTo[MasterOrderKeeper.Response.ForAddOrder])
        .map(_.created)

    def order(orderId: OrderId): Task[Option[Order[Order.State]]] =
      Task.deferFuture(
        (orderKeeper ? MasterOrderKeeper.Command.GetOrder(orderId)).mapTo[Option[Order[Order.State]]])

    def orders: Task[Stamped[Seq[Order[Order.State]]]] =
      Task.deferFuture(
        (orderKeeper ? MasterOrderKeeper.Command.GetOrders).mapTo[Stamped[Seq[Order[Order.State]]]])

    def orderCount =
      Task.deferFuture(
        (orderKeeper ? MasterOrderKeeper.Command.GetOrderCount).mapTo[Int])
  }
}
