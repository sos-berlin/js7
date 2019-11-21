package com.sos.jobscheduler.master

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.Uri
import akka.pattern.ask
import akka.util.Timeout
import cats.instances.either._
import cats.syntax.flatMap._
import com.google.inject.Stage.{DEVELOPMENT, PRODUCTION}
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import com.google.inject.{Guice, Injector, Module}
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.akkautils.CatchingActor
import com.sos.jobscheduler.common.event.{EventIdClock, StrictEventWatch}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.{Closer, Logger}
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.core.command.{CommandExecutor, CommandMeta}
import com.sos.jobscheduler.core.crypt.generic.GenericSignatureVerifier
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.JournalActor.Output
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.Recovered
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.order.FreshOrder
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.master.RunningMaster._
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.Cluster
import com.sos.jobscheduler.master.command.MasterCommandExecutor
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterSnapshots.SnapshotJsonCodec
import com.sos.jobscheduler.master.data.events.MasterKeyedEventJsonCodec
import com.sos.jobscheduler.master.web.MasterWebServer
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, blocking}
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
  val eventWatch: StrictEventWatch,
  webServer: MasterWebServer,
  val fileBasedApi: MainFileBasedApi,
  val orderApi: OrderApi.WithCommands,
  commandExecutor: MasterCommandExecutor,
  terminated1: Future[Completed],
  closer: Closer,
  val injector: Injector)
extends AutoCloseable
{
  implicit val scheduler = injector.instance[Scheduler]
  val config: Config = injector.instance[Config]
  val sessionRegister: SessionRegister[SimpleSession] = injector.instance[SessionRegister[SimpleSession]]

  @TestOnly
  lazy val actorSystem = injector.instance[ActorSystem]

  val terminated: Future[Completed] =
    for (o <- terminated1) yield {
      close()
      o
    }

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
            _ <- executeCommandAsSystemUser(MasterCommand.ShutDown) map (_.orThrow)
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
  def runOrder(order: FreshOrder): Unit = {
    val eventId = eventWatch.lastAddedEventId
    addOrderBlocking(order)
    eventWatch.await[OrderFinished](_.key == order.id, after = eventId)
  }

  @TestOnly
  def addOrderBlocking(order: FreshOrder): Boolean =
    orderApi.addOrder(order).runToFuture.await(99.s).orThrow

  val localUri: Uri = webServer.localUri
  lazy val httpApi: HttpMasterApi = new AkkaHttpMasterApi.CommonAkka {
      protected def baseUri = localUri
      protected def actorSystem = RunningMaster.this.actorSystem
    } closeWithCloser closer

  @TestOnly
  def journalActorState: Output.State =
    (actorSystem.actorSelection("user/Journal") ? JournalActor.Input.GetState)(Timeout(99.s))
      .mapTo[JournalActor.Output.State]
      .await(99.s)

  def close() = closer.close()
}

object RunningMaster
{
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
    fromInjector(Guice.createInjector(PRODUCTION, new MasterModule(configuration)))

  def fromInjector(injector: Injector): Future[RunningMaster] =
    new Starter(injector).start()

  private class Starter(injector: Injector)
  {
    private val runningSince = now
    private val masterConfiguration = injector.instance[MasterConfiguration]
    private val journalMeta = JournalMeta(SnapshotJsonCodec, MasterKeyedEventJsonCodec, masterConfiguration.journalFileBase)
    private implicit val scheduler = injector.instance[Scheduler]
    private implicit lazy val closer = injector.instance[Closer]
    private implicit lazy val actorSystem = injector.instance[ActorSystem]

    import masterConfiguration.akkaAskTimeout

    private[RunningMaster] def start(): Future[RunningMaster] = {
      val whenRecovered = Future {  // May take minutes !!!
        MasterJournalRecoverer.recover(journalMeta, masterConfiguration.config)
      }
      val journalActor = tag[JournalActor.type](actorSystem.actorOf(
        JournalActor.props(journalMeta, runningSince,
          masterConfiguration.journalConf, injector.instance[StampedKeyedEventBus], scheduler, injector.instance[EventIdClock]),
        "Journal"))
      val cluster = new Cluster(
        journalMeta,
        new JournaledStatePersistence[ClusterState, ClusterEvent](journalActor).closeWithCloser,
        masterConfiguration.clusterConf,
        actorSystem)

      val recovered = Await.result(whenRecovered, Duration.Inf).closeWithCloser
      val (orderKeeper, orderKeeperStopped) = startMasterOrderKeeper(journalActor, cluster, recovered)
      val terminated = orderKeeperStopped
        .andThen { case Failure(t) => logger.error(t.toStringWithCauses, t) }
        .andThen { case _ => closer.close() }  // Close automatically after termination

      val fileBasedApi = new MainFileBasedApi(masterConfiguration, orderKeeper)
      val orderApi = new MainOrderApi(orderKeeper)
      val commandExecutor = new MasterCommandExecutor(
        new CommandExecutor[MasterCommand] {
          def executeCommand(command: MasterCommand, meta: CommandMeta) =
            Task.deferFuture(
              (orderKeeper ? MasterOrderKeeper.Command.Execute(command, meta))
                .mapTo[Checked[command.Response]])
        },
        () => cluster)

      val webServer = injector.instance[MasterWebServer.Factory].apply(
        fileBasedApi, orderApi, commandExecutor,
        masterStateTask(orderKeeper),
        totalRunningTimeTask(orderKeeper).map(_ + runningSince.elapsed),
        recovered.eventWatch
      ).closeWithCloser

      for (_ <- webServer.start()) yield {
        createSessionTokenFile(injector.instance[SessionRegister[SimpleSession]])
        masterConfiguration.stateDirectory / "http-uri" := webServer.localHttpUri.fold(_ => "", _ + "/master")
        new RunningMaster(recovered.eventWatch.strict, webServer, fileBasedApi, orderApi, commandExecutor,
          terminated, closer, injector)
      }
    }

    private def createSessionTokenFile(sessionRegister: SessionRegister[SimpleSession]): Unit = {
      val sessionTokenFile = masterConfiguration.stateDirectory / "session-token"
      blocking {
        sessionRegister.createSystemSession(SimpleUser.System, sessionTokenFile)
          .runToFuture await masterConfiguration.akkaAskTimeout.duration
      }
      closer onClose { sessionTokenFile.delete() }
    }

    private def startMasterOrderKeeper(journalActor: ActorRef @@ JournalActor.type, cluster: Cluster,
      recovered: Recovered[MasterState, Event])
    : (ActorRef @@ MasterOrderKeeper.type, Future[Completed]) = {
      val (actor, stopped) =
        CatchingActor.actorOf[Completed](
            _ => Props {
              new MasterOrderKeeper(
                journalActor,
                cluster,
                recovered.eventWatch,
                masterConfiguration,
                GenericSignatureVerifier(masterConfiguration.config).orThrow)(
                scheduler)
            },
            "MasterOrderKeeper",
            onStopped = _ => Success(Completed)
          )
      actor ! MasterOrderKeeper.Input.Start(recovered)
      (tag[MasterOrderKeeper.type](actor), stopped)
    }
  }

  private def masterStateTask(orderKeeper: ActorRef)(implicit akkaAskTimeout: Timeout): Task[MasterState] =
    Task.deferFuture(
      (orderKeeper ? MasterOrderKeeper.Command.GetState).mapTo[MasterState])

  private def totalRunningTimeTask(orderKeeper: ActorRef)(implicit akkaAskTimeout: Timeout): Task[FiniteDuration] =
    Task.deferFuture(
      (orderKeeper ? MasterOrderKeeper.Command.GetTotalRunningTime).mapTo[FiniteDuration])
}
