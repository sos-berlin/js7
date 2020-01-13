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
import com.sos.jobscheduler.core.problems.{ClusterNodeIsNotActiveProblem, ClusterNodeIsStillStartingProblem, JobSchedulerIsShuttingDownProblem}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.order.FreshOrder
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.master.RunningMaster._
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.{Cluster, ClusterFollowUp}
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
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise, blocking}
import scala.util.control.NoStackTrace
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
  terminated1: Future[MasterTermination],
  closer: Closer,
  val injector: Injector)
extends AutoCloseable
{
  implicit val scheduler = injector.instance[Scheduler]
  val config: Config = injector.instance[Config]
  val sessionRegister: SessionRegister[SimpleSession] = injector.instance[SessionRegister[SimpleSession]]

  @TestOnly
  lazy val actorSystem = injector.instance[ActorSystem]

  val terminated: Future[MasterTermination] =
    for (o <- terminated1) yield {
      close()
      o
    }

  def terminate(): Task[MasterTermination] =
    if (terminated.isCompleted)  // Works only if previous termination has been completed
      Task.fromFuture(terminated)
    else
      injector.instance[ActorSystem].whenTerminated.value match {
        case Some(Failure(t)) => Task.raiseError(t)
        case Some(Success(_)) =>
          logger.warn("Master terminate: Akka has already been terminated")
          Task.pure(MasterTermination.Terminate(restart = false))
        case None =>
          logger.debug("terminate")
          for {
            _ <- executeCommandAsSystemUser(MasterCommand.ShutDown()) map (_.orThrow)
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
      protected val baseUri = localUri
      protected val name = "RunningMaster"
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
        JournalActor.props(journalMeta, masterConfiguration.journalConf,
          injector.instance[StampedKeyedEventBus], scheduler, injector.instance[EventIdClock]),
        "Journal"))
      val cluster = new Cluster(
        journalMeta,
        new JournaledStatePersistence[ClusterState, ClusterEvent](journalActor).closeWithCloser,
        masterConfiguration.clusterConf,
        actorSystem)
      val recovered = Await.result(whenRecovered, Duration.Inf).closeWithCloser

      // startingClusterFuture terminates when this cluster node becomes active or terminates
      // maybePassiveState accesses the current MasterState while this node is passive, otherwise it is None
      val (startingClusterFuture, maybePassiveState) = startCluster(journalActor, cluster, recovered)
      val (orderKeeperStarted, orderKeeperTerminated) = {
        val started = startingClusterFuture.map(_.flatMap(
          startMasterOrderKeeper(journalActor, cluster, _)))
        (started.map(_.map(_.actor)),
          started.flatMap {
            case None => Future.successful(MasterTermination.Terminate(restart = false))
            case Some(o) =>
              o.termination andThen { case tried =>
                for (t <- tried.failed) {
                  logger.error(s"MasterOrderKeeper failed with ${t.toStringWithCauses}", t)  // Support diagnosis
                }
                startingClusterFuture.cancel()
              }
          })
      }
      val orderKeeperTask = Task.fromFuture(orderKeeperStarted) flatMap {
        case None => Task.raiseError(JobSchedulerIsShuttingDownProblem.throwable)
        case Some(actor) => Task.pure(actor)
      }
      val commandExecutor = new MasterCommandExecutor(new MyCommandExecutor(startingClusterFuture, orderKeeperStarted, cluster))
      val fileBasedApi = new MainFileBasedApi(masterConfiguration, orderKeeperTask)
      val orderApi = new MainOrderApi(orderKeeperTask)

      val webServer = injector.instance[MasterWebServer.Factory].apply(fileBasedApi, orderApi, commandExecutor,
        masterState =
          orderKeeperStarted.value/*Future completed?*/ match {
            case None =>
              maybePassiveState match {
                case None => Task.pure(Left(ClusterNodeIsStillStartingProblem))
                case Some(state) => state map Right.apply
              }
            case Some(Failure(t)) => Task.raiseError(t)
            case Some(Success(None)) => Task.pure(Left(JobSchedulerIsShuttingDownProblem))
            case Some(Success(Some(actor))) =>
              Task.deferFuture(
                (actor ? MasterOrderKeeper.Command.GetState).mapTo[MasterState]
              ).map(Right.apply)
          },
        recovered.totalRunningSince,  // Maybe different from JournalHeader
        recovered.eventWatch
      ).closeWithCloser

      for (_ <- webServer.start()) yield {
        createSessionTokenFile(injector.instance[SessionRegister[SimpleSession]])
        masterConfiguration.stateDirectory / "http-uri" := webServer.localHttpUri.fold(_ => "", _ + "/master")
        new RunningMaster(recovered.eventWatch.strict, webServer, fileBasedApi, orderApi, commandExecutor,
          orderKeeperTerminated, closer, injector)
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

    /** @return Task(None) when canceled. */
    private def startCluster(
      journalActor: ActorRef @@ JournalActor.type,
      cluster: Cluster,
      recovered: Recovered[MasterState, Event])
    : (CancelableFuture[Option[ClusterFollowUp[MasterState, Event]]], Option[Task[MasterState]]) =
    {
      class StartingClusterCanceledException extends NoStackTrace
      val recoveredState = recovered.recoveredState getOrElse MasterState.Undefined
      val (followUpTask, maybePassiveState) = cluster.start(recovered, recoveredState.clusterState, recoveredState)
      val startingClusterFuture = followUpTask
        .map(_.orThrow)
        .doOnCancel(Task { logger.debug("Cancel Cluster") })
        .onCancelRaiseError(new StartingClusterCanceledException)
        .map(Some.apply)
        .onErrorRecoverWith {
          case _: StartingClusterCanceledException => Task.pure(None)
        }
        .executeWithOptions(_.enableAutoCancelableRunLoops)
        .runToFuture
      (startingClusterFuture, maybePassiveState)
    }

    private def startMasterOrderKeeper(
      journalActor: ActorRef @@ JournalActor.type,
      cluster: Cluster,
      followUp: ClusterFollowUp[MasterState, Event])
    : Option[OrderKeeperStarted] =
      followUp match {
        case _: ClusterFollowUp.Terminate[MasterState, Event] =>
          None

        case ClusterFollowUp.BecomeActive(recovered: Recovered[MasterState @unchecked, Event]) =>
          val terminationPromise = Promise[MasterTermination]()
          val actor = actorSystem.actorOf(
            Props {
              new MasterOrderKeeper(terminationPromise, journalActor, cluster, recovered.eventWatch, masterConfiguration,
                GenericSignatureVerifier(masterConfiguration.config).orThrow)
            },
            "MasterOrderKeeper")
          actor ! MasterOrderKeeper.Input.Start(recovered)
          val termination = terminationPromise.future
            .andThen { case Failure(t) => logger.error(t.toStringWithCauses, t) }
            .andThen { case _ => closer.close() }  // Close automatically after termination
          Some(OrderKeeperStarted(tag[MasterOrderKeeper](actor), termination))
      }
  }

  private class MyCommandExecutor(
    startingClusterFuture: Cancelable,
    orderKeeperStarted: Future[Option[ActorRef @@ MasterOrderKeeper]],
    cluster: Cluster)
    (implicit timeout: Timeout)
  extends CommandExecutor[MasterCommand]
  {
    def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[command.Response]] =
      (command match {
        case _: MasterCommand.ShutDown =>
          Task { startingClusterFuture.cancel() } >>
            Task.deferFutureAction(implicit s =>
              orderKeeperStarted flatMap {
                case None =>  // MasterOrderKeeper does not start
                  Future.successful(Right(MasterCommand.Response.Accepted))
                case Some(actor) =>
                  (actor ? MasterOrderKeeper.Command.Execute(command, meta))
                    .mapTo[Checked[command.Response]]
              })

        case MasterCommand.ClusterAppointBackup(activeUri, backupUri) =>
          cluster.appointBackupNode(activeUri, backupUri)
            .map(_.map((_: Completed) => MasterCommand.Response.Accepted))

        case MasterCommand.ClusterPassiveFollows(activeUri, followingUri) =>
          cluster.passiveNodesFollows(activeUri, followingUri)
            .map(_.map((_: Completed) => MasterCommand.Response.Accepted))

        case _ =>
          orderKeeperStarted.value match {
            case None =>  // Cluster node is still waiting for activation
              Task.pure(Left(ClusterNodeIsNotActiveProblem))
            case Some(Failure(t)) =>
              Task.raiseError(t)
            case Some(Success(None)) =>   // MasterOrderKeeper does not start
              Task.pure(Left(JobSchedulerIsShuttingDownProblem))
            case Some(Success(Some(actor))) =>
              Task.deferFutureAction(implicit s =>
                (actor ? MasterOrderKeeper.Command.Execute(command, meta))
                  .mapTo[Checked[command.Response]])
          }
      }).map(_.map((_: MasterCommand.Response).asInstanceOf[command.Response]))
  }

  private case class OrderKeeperStarted(actor: ActorRef @@ MasterOrderKeeper, termination: Future[MasterTermination])
}
