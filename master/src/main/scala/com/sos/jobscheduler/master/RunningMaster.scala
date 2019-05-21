package com.sos.jobscheduler.master

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.Uri
import akka.pattern.ask
import akka.util.Timeout
import com.google.inject.Stage.{DEVELOPMENT, PRODUCTION}
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import com.google.inject.{Guice, Injector, Module}
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichPartialFunction, RichThrowable}
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.akkautils.CatchingActor
import com.sos.jobscheduler.common.event.{EventIdClock, EventWatch, StrictEventWatch}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.{Closer, Logger}
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.core.command.{CommandExecutor, CommandMeta}
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.crypt.generic.GenericSignatureVerifier
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.filebased.{FileBasedApi, Repo}
import com.sos.jobscheduler.core.startup.StartUp
import com.sos.jobscheduler.data.event.{Event, Stamped}
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, FileBasedsOverview, TypedPath}
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId}
import com.sos.jobscheduler.master.RunningMaster._
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.command.MasterCommandExecutor
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.web.MasterWebServer
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Path
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{Future, blocking}
import scala.util.{Failure, Success}
import shapeless.tag
import shapeless.tag.@@

/**
 * JobScheduler Master.
 *
 * Integration test in engine-tests, for example com.sos.jobscheduler.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class RunningMaster private(
  val sessionRegister: SessionRegister[SimpleSession],
  val commandExecutor: MasterCommandExecutor,
  val webServer: MasterWebServer,
  val fileBasedApi: MainFileBasedApi,
  val orderApi: OrderApi.WithCommands,
  val orderKeeper: ActorRef,
  val terminated: Future[Completed],
  closer: Closer,
  val injector: Injector)
extends AutoCloseable
{
  implicit val scheduler = injector.instance[Scheduler]
  val config: Config = injector.instance[Config]

  def terminate(): Task[Completed] =
    if (terminated.isCompleted)  // Works only if previous termination has been completed
      Task.fromFuture(terminated)
    else
      injector.instance[ActorSystem].whenTerminated.value match {
        case Some(Failure(t)) => Task.raiseError(t)
        case Some(Success(_)) =>
          logger.warn("Master terminate: Akka has already been terminated")
          Task.pure(Completed)
        case None =>
          logger.debug("terminate")
          for {
            _ <- executeCommandAsSystemUser(MasterCommand.Terminate) map (_.orThrow)
            t <- Task.fromFuture(terminated)
          } yield t
      }

  def executeCommandForTest(command: MasterCommand): Checked[command.Response] =
    executeCommandAsSystemUser(command) await 99.seconds

  def executeCommandAsSystemUser(command: MasterCommand): Task[Checked[command.Response]] =
    for {
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.map(session => executeCommand(command, CommandMeta(session.currentUser))).evert
    } yield checkedChecked.flatten

  def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    commandExecutor.executeCommand(command, meta)

  def addOrder(order: FreshOrder): Task[Checked[Boolean]] =
    orderApi.addOrder(order)

  @TestOnly
  def addOrderBlocking(order: FreshOrder): Boolean =
    orderApi.addOrder(order).runToFuture.await(99.s).orThrow

  val localUri: Uri = webServer.localUri
  lazy val httpApi: HttpMasterApi = new AkkaHttpMasterApi.CommonAkka {
      protected def baseUri = localUri
      protected def actorSystem = injector.instance[ActorSystem]
    } closeWithCloser closer
  val eventWatch: StrictEventWatch[Event] = injector.instance[EventWatch[Event]].strict

  def close() = closer.close()
}

object RunningMaster
{
  val StartedAt = Timestamp.now
  private val logger = Logger(getClass)

  def run[A](configuration: MasterConfiguration, timeout: Option[FiniteDuration] = None)(body: RunningMaster => Unit)(implicit s: Scheduler): Unit =
    autoClosing(apply(configuration) await timeout) { master =>
      for (t <- master.terminated.failed) logger.error(t.toStringWithCauses, t)
      body(master)
      master.terminated await timeout
    }

  def newInjectorForTest(directory: Path, module: Module = EMPTY_MODULE,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None,
    mutualHttps: Boolean = false,
    name: String)
  : Injector =
    Guice.createInjector(DEVELOPMENT,
      Modules `override` new MasterModule(MasterConfiguration.forTest(
        configAndData = directory,
        config,
        httpPort = httpPort,
        httpsPort = httpsPort,
        mutualHttps = mutualHttps,
        name = name))
      `with` module)

  def apply(configuration: MasterConfiguration): Future[RunningMaster] =
    apply(new MasterModule(configuration))

  private def apply(module: Module): Future[RunningMaster] =
    apply(Guice.createInjector(PRODUCTION, module))

  def apply(injector: Injector): Future[RunningMaster] =
    new Starter(injector).start()

  private class Starter(injector: Injector)
  {
    // Lazy vals to allow earlier logStartUp message
    private lazy val closer = injector.instance[Closer]
    private lazy val masterConfiguration = injector.instance[MasterConfiguration]
    private lazy val actorSystem = injector.instance[ActorSystem]
    implicit private lazy val scheduler = injector.instance[Scheduler]

    private def createSessionTokenFile(sessionRegister: SessionRegister[SimpleSession]): Unit = {
      val sessionTokenFile = masterConfiguration.stateDirectory / "session-token"
      blocking {
        sessionRegister.createSystemSession(SimpleUser.System, sessionTokenFile)
          .runToFuture await masterConfiguration.akkaAskTimeout.duration
      }
      closer onClose { sessionTokenFile.delete() }
    }

    private def startMasterOrderKeeper(signatureVerifier: SignatureVerifier)
    : (ActorRef @@ MasterOrderKeeper.type, Future[Completed]) = {
      val (actor, whenCompleted) =
        CatchingActor.actorOf[Completed](
            _ => Props {
              new MasterOrderKeeper(
                masterConfiguration,
                injector.instance[JournalMeta[Event]],
                injector.instance[JournalEventWatch[Event]],
                injector.instance[EventIdClock],
                signatureVerifier)(
                injector.instance[StampedKeyedEventBus],
                scheduler)
            },
            "MasterOrderKeeper",
            onStopped = _ => Success(Completed)
          )(actorSystem)
      (tag[MasterOrderKeeper.type](actor), whenCompleted)
    }

    private[RunningMaster] def start(): Future[RunningMaster] = {
      StartUp.logStartUp(masterConfiguration.configDirectory, Some(masterConfiguration.dataDirectory))
      if (!exists(masterConfiguration.stateDirectory)) {  // In case of a test
        createDirectory(masterConfiguration.stateDirectory)
      }

      val sessionRegister = injector.instance[SessionRegister[SimpleSession]]
      createSessionTokenFile(sessionRegister)

      val signatureVerifier = GenericSignatureVerifier(masterConfiguration.config).orThrow
      val (orderKeeper, orderKeeperStopped) = startMasterOrderKeeper(signatureVerifier)
      val fileBasedApi = new MainFileBasedApi(masterConfiguration, orderKeeper)
      val orderKeeperCommandExecutor = new CommandExecutor[MasterCommand] {
        def executeCommand(command: MasterCommand, meta: CommandMeta) =
          Task.deferFuture(
            (orderKeeper ? MasterOrderKeeper.Command.Execute(command, meta))(masterConfiguration.akkaAskTimeout)
              .mapTo[Checked[command.Response]])
      }
      val commandExecutor = new MasterCommandExecutor(orderKeeperCommandExecutor)
      val orderApi = new MainOrderApi(orderKeeper, masterConfiguration.akkaAskTimeout)
      val masterState = getMasterState(orderKeeper, masterConfiguration.akkaAskTimeout)

      val terminated = orderKeeperStopped
        .andThen { case Failure(t) => logger.error(t.toStringWithCauses, t) }
        .andThen { case _ => closer.close() }  // Close automatically after termination

      val webServer = injector.instance[MasterWebServer.Factory].apply(fileBasedApi, orderApi, commandExecutor, masterState)
      masterConfiguration.stateDirectory / "http-uri" := webServer.localHttpUri.fold(_ => "", _ + "/master")
      for (_ <- webServer.start()) yield
        new RunningMaster(sessionRegister, commandExecutor, webServer, fileBasedApi, orderApi, orderKeeper, terminated, closer, injector)
    }
  }

  final class MainFileBasedApi(masterConfiguration: MasterConfiguration, orderKeeper: ActorRef) extends FileBasedApi
  {
    def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Stamped[O.Overview]] =
      for (stamped <- stampedRepo) yield
        for (repo <- stamped) yield
          O.fileBasedsToOverview(repo.currentTyped[A].values.toImmutableSeq)

    def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
      for (stamped <- stampedRepo) yield
        for (repo <- stamped) yield
          repo.idTo[A](id)

    def fileBaseds[A <: FileBased: FileBased.Companion]: Task[Stamped[Seq[A]]] =
      for (stamped <- stampedRepo) yield
        for (repo <- stamped) yield
          repo.currentTyped[A].values.toImmutableSeq.sortBy/*for determinstic tests*/(_.id: FileBasedId[TypedPath])

    def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path): Task[Checked[Stamped[A]]] =
      for (stamped <- stampedRepo; repo = stamped.value) yield
        for (a <- repo.currentTyped[A].checked(path)) yield
          stamped.copy(value = a)

    def stampedRepo: Task[Stamped[Repo]] = {
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

    def addOrders(order: Seq[FreshOrder]) =
      Task.deferFuture(
        (orderKeeper ? MasterOrderKeeper.Command.AddOrders(order)).mapTo[Checked[Completed]])

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

  private def getMasterState(orderKeeper: ActorRef, akkaAskTimeout: Timeout): Task[MasterState] = {
    implicit def t = akkaAskTimeout
    Task.deferFuture(
      (orderKeeper ? MasterOrderKeeper.Command.GetState).mapTo[MasterState])
  }
}
