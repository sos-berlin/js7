package com.sos.jobscheduler.master

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import cats.syntax.flatMap._
import com.google.inject.Stage.{DEVELOPMENT, PRODUCTION}
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import com.google.inject.{Guice, Injector, Module}
import com.sos.jobscheduler.base.auth.{SimpleUser, UserAndPassword}
import com.sos.jobscheduler.base.eventbus.{EventPublisher, StandardEventBus}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.base.utils.Closer.syntax.RichClosersAutoCloseable
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, RichThrowable}
import com.sos.jobscheduler.base.utils.{Closer, SetOnce}
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.event.{EventIdGenerator, StrictEventWatch}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.core.command.{CommandExecutor, CommandMeta}
import com.sos.jobscheduler.core.crypt.generic.GenericSignatureVerifier
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.JournalActor.Output
import com.sos.jobscheduler.core.event.journal.recover.Recovered
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence
import com.sos.jobscheduler.core.problems.{ClusterNodeIsNotActiveProblem, ClusterNodeIsNotYetReadyProblem, JobSchedulerIsShuttingDownProblem}
import com.sos.jobscheduler.data.Problems.PassiveClusterNodeShutdownNotAllowedProblem
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.{EventRequest, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent}
import com.sos.jobscheduler.master.RunningMaster._
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.{Cluster, ClusterFollowUp}
import com.sos.jobscheduler.master.command.MasterCommandExecutor
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.data.{MasterCommand, MasterState}
import com.sos.jobscheduler.master.problems.MasterIsNotYetReadyProblem
import com.sos.jobscheduler.master.web.MasterWebServer
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.deleteIfExists
import java.nio.file.Path
import monix.eval.Task
import monix.execution.Scheduler
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
  val clusterState: Task[ClusterState],
  commandExecutor: MasterCommandExecutor,
  whenReady: Future[MasterOrderKeeper.MasterReadyTestIncident.type],
  terminated1: Future[MasterTermination],
  val testEventBus: StandardEventBus[Any],
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
  def runOrder(order: FreshOrder): Seq[Stamped[OrderEvent]] = {
    val timeout = 99.s
    val eventId = eventWatch.lastAddedEventId
    addOrderBlocking(order)
    eventWatch
      .observe(EventRequest.singleClass[OrderEvent](eventId, Some(timeout + 9.s)))
      .takeWhile(_.value.key == order.id)
      .map(o => o.copy(value = o.value.event))
      .takeWhileInclusive(o => !o.value.isInstanceOf[OrderFinished])
      .toListL
      .await(timeout)
  }

  @TestOnly
  def addOrderBlocking(order: FreshOrder): Boolean =
    orderApi.addOrder(order).runToFuture.await(99.s).orThrow

  @TestOnly
  def waitUntilReady(): Unit =
    whenReady await 99.s

  @TestOnly
  lazy val localUri = webServer.localUri

  private val httpApiUserAndPassword = SetOnce[Option[UserAndPassword]]
  private val _httpApi = SetOnce[HttpMasterApi]

  @TestOnly
  def httpApiDefaultLogin(userAndPassword: Option[UserAndPassword]): Unit = {
    assertThat(_httpApi.isEmpty)
    httpApiUserAndPassword := userAndPassword
    httpApi
  }

  @TestOnly
  lazy val httpApi: HttpMasterApi = {
    if (_httpApi.isEmpty) {
      httpApiUserAndPassword.trySet(None)
      _httpApi := new AkkaHttpMasterApi(localUri, httpApiUserAndPassword.get, actorSystem = actorSystem, config = config)
        .closeWithCloser(closer)
    }
    _httpApi.get
  }


  @TestOnly
  def journalActorState: Output.JournalActorState =
    (actorSystem.actorSelection("user/Journal") ? JournalActor.Input.GetJournalActorState)(Timeout(99.s))
      .mapTo[JournalActor.Output.JournalActorState]
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
    private implicit val scheduler = injector.instance[Scheduler]
    private implicit lazy val closer = injector.instance[Closer]
    private implicit lazy val actorSystem = injector.instance[ActorSystem]
    private lazy val signatureVerifier = GenericSignatureVerifier(masterConfiguration.config).orThrow
    import masterConfiguration.{akkaAskTimeout, journalMeta}
    @volatile private var clusterStartupTermination = MasterTermination.Terminate()

    private[RunningMaster] def start(): Future[RunningMaster] = {
      val whenRecovered = Future {  // May take minutes !!!
        MasterJournalRecoverer.recover(journalMeta, masterConfiguration.config)
      }
      val testEventBus = injector.instance[StandardEventBus[Any]]
      val whenReady = testEventBus.when[MasterOrderKeeper.MasterReadyTestIncident.type].runToFuture  // TODO Replace by a new StampedEventBus ?
      // Start-up some stuff while recovering
      val journalActor = tag[JournalActor.type](actorSystem.actorOf(
        JournalActor.props[MasterState](journalMeta, masterConfiguration.journalConf,
          injector.instance[StampedKeyedEventBus], scheduler, injector.instance[EventIdGenerator],
          useJournaledStateAsSnapshot = true),
        "Journal"))
      signatureVerifier
      val persistence = new JournaledStatePersistence[MasterState](journalActor).closeWithCloser
      val cluster = new Cluster(
        journalMeta,
        persistence,
        masterConfiguration.masterId,
        masterConfiguration.journalConf,
        masterConfiguration.clusterConf,
        masterConfiguration.config,
        injector.instance[EventIdGenerator],
        testEventBus)
      val recovered = Await.result(whenRecovered, Duration.Inf).closeWithCloser

      // clusterFollowUpFuture terminates when this cluster node becomes active or terminates
      // maybePassiveState accesses the current MasterState while this node is passive, otherwise it is None
      val (currentPassiveMasterState, clusterFollowUpTask) = startCluster(cluster, recovered)
      val clusterFollowUpFuture = clusterFollowUpTask
        .flatTap {
          case Right(ClusterFollowUp.BecomeActive(recovered)) =>
            Task { persistence.start(recovered.state) }
          case _ => Task.unit
        }
        .executeWithOptions(_.enableAutoCancelableRunLoops)
        .runToFuture
      val (orderKeeperStarted, orderKeeperTerminated) = {
        val started = clusterFollowUpFuture.map(_.flatMap(
          startMasterOrderKeeper(journalActor, cluster, _, testEventBus)))
        (started.map(_.map(_.actor)),
          started.flatMap {
            case Left(termination) =>
              Future.successful(termination)
            case Right(o) =>
              o.termination andThen { case tried =>
                for (t <- tried.failed) {
                  logger.error(s"MasterOrderKeeper failed with ${t.toStringWithCauses}", t)  // Support diagnosis
                }
                clusterFollowUpFuture.cancel()
              }
          })
      }
      for (t <- orderKeeperStarted.failed) logger.debug("orderKeeperStarted => " + t.toStringWithCauses, t)
      //for (t <- orderKeeperTerminated.failed) logger.debug("orderKeeperTerminated => " + t.toStringWithCauses, t)
      val orderKeeperTask = Task.defer {
        orderKeeperStarted.value match {
          case None => Task.raiseError(MasterIsNotYetReadyProblem.throwable)
          case Some(orderKeeperTry) =>
            orderKeeperTry match {
              case Failure(t) => Task.raiseError(t)
              case Success(Left(_)) => Task.raiseError(JobSchedulerIsShuttingDownProblem.throwable)
              case Success(Right(actor)) => Task.pure(actor)
            }
        }
      }
      val commandExecutor = new MasterCommandExecutor(
        new MyCommandExecutor(cluster,
          onShutDownPassive = termination => Task {
            clusterStartupTermination = termination
            clusterFollowUpFuture.cancel()
          },
          orderKeeperStarted.map(_.toOption)))
      val masterState = Task.defer {
        if (persistence.isStarted)
          persistence.currentState map Right.apply
        else
          currentPassiveMasterState.map(_.toChecked(ClusterNodeIsNotYetReadyProblem))
      }
      val fileBasedApi = new MainFileBasedApi(masterState)
      val orderApi = new MainOrderApi(masterState, orderKeeperTask)

      val webServer = injector.instance[MasterWebServer.Factory]
        .apply(fileBasedApi, orderApi, commandExecutor,
          cluster.currentClusterState,
          masterState,
          recovered.totalRunningSince,  // Maybe different from JournalHeader
          recovered.eventWatch
        ).closeWithCloser

      for (_ <- webServer.start().runToFuture) yield {
        createSessionTokenFile(injector.instance[SessionRegister[SimpleSession]])
        masterConfiguration.stateDirectory / "http-uri" := webServer.localHttpUri.fold(_ => "", o => s"$o/master")
        new RunningMaster(recovered.eventWatch.strict, webServer, fileBasedApi, orderApi, cluster.currentClusterState,
          commandExecutor,
          whenReady, orderKeeperTerminated, testEventBus, closer, injector)
      }
    }

    private def createSessionTokenFile(sessionRegister: SessionRegister[SimpleSession]): Unit = {
      val sessionTokenFile = masterConfiguration.stateDirectory / "session-token"
      blocking {
        sessionRegister.createSystemSession(SimpleUser.System, sessionTokenFile)
          .runToFuture await masterConfiguration.akkaAskTimeout.duration
      }
      closer onClose { deleteIfExists(sessionTokenFile) }
    }

    /** @return Task(None) when cancelled. */
    private def startCluster(
      cluster: Cluster,
      recovered: Recovered[MasterState])
    : (Task[Option[MasterState]], Task[Either[MasterTermination.Terminate, ClusterFollowUp[MasterState]]]) =
    {
      class StartingClusterCancelledException extends NoStackTrace
      val recoveredState = recovered.recoveredState getOrElse MasterState.Undefined
      val (passiveState, followUpTask) = cluster.start(recovered, recoveredState)
      passiveState ->
        followUpTask
          .doOnCancel(Task { logger.debug("Cancel Cluster") })
          .onCancelRaiseError(new StartingClusterCancelledException)
          .map(_.orThrow)
          .map(Right.apply)
          .onErrorRecoverWith {
            case _: StartingClusterCancelledException => Task { Left(clusterStartupTermination) }
          }
    }

    private def startMasterOrderKeeper(
      journalActor: ActorRef @@ JournalActor.type,
      cluster: Cluster,
      followUp: ClusterFollowUp[MasterState],
      testEventPublisher: EventPublisher[Any])
    : Either[MasterTermination.Terminate, OrderKeeperStarted] = {
      logger.debug(s"startMasterOrderKeeper(clusterFollowUp=${followUp.getClass.simpleScalaName})")
      followUp match {
        //case _: ClusterFollowUp.Terminate[MasterState, Event] =>
        //  Left(MasterTermination.Terminate(restart = false))

        case ClusterFollowUp.BecomeActive(recovered: Recovered[MasterState @unchecked]) =>
          val terminationPromise = Promise[MasterTermination]()
          val actor = actorSystem.actorOf(
            Props {
              new MasterOrderKeeper(terminationPromise, journalActor, cluster, masterConfiguration,
                signatureVerifier, testEventPublisher)
            },
            "MasterOrderKeeper")
          actor ! MasterOrderKeeper.Input.Start(recovered)
          val termination = terminationPromise.future
            .andThen { case Failure(t) => logger.error(t.toStringWithCauses, t) }
            .andThen { case _ => closer.close() }  // Close automatically after termination
          Right(OrderKeeperStarted(tag[MasterOrderKeeper](actor), termination))
      }
    }
  }

  private class MyCommandExecutor(
    cluster: Cluster,
    onShutDownPassive: MasterTermination.Terminate => Task[Unit],
    orderKeeperStarted: Future[Option[ActorRef @@ MasterOrderKeeper]])
    (implicit timeout: Timeout)
  extends CommandExecutor[MasterCommand]
  {
    def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[command.Response]] =
      (command match {
        case command: MasterCommand.ShutDown =>
          cluster.isActive.flatMap(isActive =>
            if (!isActive)
              if (command.clusterAction.isEmpty)
                onShutDownPassive(MasterTermination.Terminate(restart = command.restart))
                  .map(_ => Right(MasterCommand.Response.Accepted))
              else
                Task.pure(Left(PassiveClusterNodeShutdownNotAllowedProblem))
            else
              Task.deferFutureAction(implicit s =>
                orderKeeperStarted flatMap {
                  case None =>  // MasterOrderKeeper does not start
                    Future.successful(
                      if (command.clusterAction.nonEmpty)
                        Left(PassiveClusterNodeShutdownNotAllowedProblem)
                      else
                        Right(MasterCommand.Response.Accepted))
                  case Some(actor) =>
                    (actor ? MasterOrderKeeper.Command.Execute(command, meta))
                      .mapTo[Checked[command.Response]]
                }))

        case MasterCommand.ClusterAppointNodes(idToUri, activeId) =>
          cluster.appointNodes(idToUri, activeId)
            .map(_.map((_: Completed) => MasterCommand.Response.Accepted))

        case MasterCommand.InternalClusterCommand(clusterCommand) =>
          cluster.executeCommand(clusterCommand)
            .map(_.map(MasterCommand.InternalClusterCommand.Response.apply))

        case _ =>
          orderKeeperStarted.value match {
            case None =>  // Cluster node is still waiting for activation
              Task.pure(Left(ClusterNodeIsNotActiveProblem))
            case Some(Failure(t)) =>
              Task.raiseError(t)
            case Some(Success(None)) =>   // MasterOrderKeeper does not start
              Task.pure(Left(JobSchedulerIsShuttingDownProblem))
            case Some(Success(Some(actor))) =>
              Task.deferFuture(
                (actor ? MasterOrderKeeper.Command.Execute(command, meta))
                  .mapTo[Checked[command.Response]])
          }
      }).map(_.map((_: MasterCommand.Response).asInstanceOf[command.Response]))
  }

  private case class OrderKeeperStarted(actor: ActorRef @@ MasterOrderKeeper, termination: Future[MasterTermination])
}
