package js7.controller

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import com.google.inject.Stage.{DEVELOPMENT, PRODUCTION}
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import com.google.inject.{Guice, Injector, Module}
import com.softwaremill.diffx.generic.auto.*
import com.softwaremill.tagging.{@@, Tagger}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.auth.{SimpleUser, UserAndPassword}
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.problem.{Checked, Problem}
import js7.base.thread.Futures.implicits.*
import js7.base.thread.IOExecutor
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.AlarmClock
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Closer, ProgramTermination, SetOnce}
import js7.cluster.ClusterNode.RestartAfterJournalTruncationException
import js7.cluster.{ClusterNode, WorkingClusterNode}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController.*
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.configuration.inject.ControllerModule
import js7.controller.item.ItemUpdater
import js7.controller.web.ControllerWebServer
import js7.core.command.{CommandExecutor, CommandMeta}
import js7.core.license.LicenseChecker
import js7.data.Problems.{ClusterNodeIsNotActiveProblem, ClusterNodeIsNotReadyProblem, PassiveClusterNodeShutdownNotAllowedProblem}
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerCommand.{AddOrder, ShutDown}
import js7.data.controller.{ControllerCommand, ControllerState, VerifiedUpdateItems}
import js7.data.crypt.SignedItemVerifier
import js7.data.event.{EventId, EventRequest, Stamped}
import js7.data.item.{ItemOperation, SignableItem, UnsignedSimpleItem}
import js7.data.order.OrderEvent.{OrderDeleted, OrderFailed, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent}
import js7.journal.JournalActor.Output
import js7.journal.recover.{Recovered, StateRecoverer}
import js7.journal.state.FileStatePersistence
import js7.journal.watch.StrictEventWatch
import js7.journal.{EventIdGenerator, JournalActor, StampedKeyedEventBus}
import js7.license.LicenseCheckContext
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future, Promise}
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

/**
 * JS7 Controller.
 *
 * Integration test in engine-tests, for example js7.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class RunningController private(
  val eventWatch: StrictEventWatch,
  webServer: AkkaWebServer & AkkaWebServer.HasUri,
  val recoveredEventId: EventId,
  val orderApi: OrderApi,
  val controllerState: Task[ControllerState],
  commandExecutor: ControllerCommandExecutor,
  itemUpdater: ItemUpdater,
  whenReady: Future[Unit],
  terminated1: Future[ProgramTermination],
  val testEventBus: StandardEventBus[Any],
  closer: Closer,
  val injector: Injector)
extends AutoCloseable
{
  implicit val scheduler: Scheduler = injector.instance[Scheduler]
  val config: Config = injector.instance[Config]
  val sessionRegister: SessionRegister[SimpleSession] = injector.instance[SessionRegister[SimpleSession]]
  private lazy val controllerConfiguration = injector.instance[ControllerConfiguration]
  private val httpApiUserAndPassword = SetOnce[Option[UserAndPassword]]
  private val _httpApi = SetOnce[AkkaHttpControllerApi]

  @TestOnly lazy val localUri = webServer.localUri
  @TestOnly lazy val httpApi: HttpControllerApi = {
    if (_httpApi.isEmpty) {
      httpApiUserAndPassword.trySet(None)
      _httpApi := new AkkaHttpControllerApi(localUri, httpApiUserAndPassword.orThrow,
        actorSystem = actorSystem, config = config, name = controllerConfiguration.name)
    }
    _httpApi.orThrow
  }

  lazy val actorSystem = injector.instance[ActorSystem]

  val terminated: Future[ProgramTermination] = {
    val t = terminated1
      .recover { case t: RestartAfterJournalTruncationException =>
        logger.info(t.getMessage)
        ProgramTermination(restart = true)
      }
    for (o <- t) yield {
      close()
      o
    }
  }

  def terminate(
    suppressSnapshot: Boolean = false,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    dontNotifyActiveNode: Boolean = false)
  : Task[ProgramTermination] =
    Task.defer {
      if (terminated.isCompleted)  // Works only if previous termination has been completed
        Task.fromFuture(terminated)
      else
        injector.instance[ActorSystem].whenTerminated.value match {
          case Some(Failure(t)) => Task.raiseError(t)
          case Some(Success(_)) =>
            logger.warn("Controller terminate: Akka has already been terminated")
            Task.pure(ProgramTermination(restart = false))
          case None =>
            logger.debug("terminate")
            for {
              _ <- _httpApi.toOption.fold(Task.unit)(_
                .tryLogout.void.onErrorHandle(t => logger.warn(t.toString)))
              _ <- executeCommandAsSystemUser(ControllerCommand
                .ShutDown(
                  suppressSnapshot = suppressSnapshot,
                  clusterAction = clusterAction,
                  dontNotifyActiveNode = dontNotifyActiveNode)
              ).map(_.orThrow)
              t <- Task.fromFuture(terminated)
            } yield t
        }
    }

  def executeCommandForTest(command: ControllerCommand): Checked[command.Response] =
    executeCommandAsSystemUser(command) await 99.s

  def executeCommandAsSystemUser(command: ControllerCommand): Task[Checked[command.Response]] =
    for {
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse(session => executeCommand(command, CommandMeta(session.currentUser)))
    } yield checkedChecked.flatten

  def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    commandExecutor.executeCommand(command, meta)

  def updateUnsignedSimpleItemsAsSystemUser(items: Seq[UnsignedSimpleItem]): Task[Checked[Completed]] =
    sessionRegister.systemUser
      .flatMapT(updateUnsignedSimpleItems(_, items))

  def updateUnsignedSimpleItems(user: SimpleUser, items: Seq[UnsignedSimpleItem]): Task[Checked[Completed]] =
    VerifiedUpdateItems
      .fromOperations(
        Observable.fromIterable(items)
          .map(ItemOperation.AddOrChangeSimple.apply),
        _ => Left(Problem.pure("updateUnsignedSimpleItems and verify?")),
        user)
      .flatMapT(itemUpdater.updateItems)

  def updateItemsAsSystemUser(operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    sessionRegister.systemUser
      .flatMapT(updateItems(_, operations))

  def updateItems(user: SimpleUser, operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    VerifiedUpdateItems
      .fromOperations(operations, itemUpdater.signedItemVerifier.verify, user)
      .flatMapT(itemUpdater.updateItems)

  @TestOnly
  def addOrderBlocking(order: FreshOrder): Unit =
    addOrder(order)
      .runToFuture.await(99.s).orThrow

  @TestOnly
  def addOrder(order: FreshOrder): Task[Checked[Unit]] =
    executeCommandAsSystemUser(AddOrder(order))
      .mapT(response =>
        (!response.ignoredBecauseDuplicate) !! Problem(s"Duplicate OrderId '${order.id}'"))

  @TestOnly
  def runOrder(order: FreshOrder): Seq[Stamped[OrderEvent]] = {
    val timeout = 99.s
    val eventId = eventWatch.lastAddedEventId
    addOrderBlocking(order)
    eventWatch
      .observe(EventRequest.singleClass[OrderEvent](eventId, Some(timeout + 9.s)))
      .filter(_.value.key == order.id)
      .map(o => o.copy(value = o.value.event))
      .takeWhileInclusive { case Stamped(_, _, event) =>
        if (order.deleteWhenTerminated)
          event != OrderDeleted && !event.isInstanceOf[OrderFailed]
        else
          !event.isInstanceOf[OrderTerminated]
      }
      .toL(Vector)
      .await(timeout)
  }

  @TestOnly
  def waitUntilReady(): Unit =
    Task.fromFuture(whenReady)
      .logWhenItTakesLonger
      .await(99.s)

  @TestOnly
  def clusterState: Task[ClusterState] =
    controllerState.map(_.clusterState)

  @TestOnly
  def httpApiDefaultLogin(userAndPassword: Option[UserAndPassword]): Unit = {
    assertThat(_httpApi.isEmpty)
    httpApiUserAndPassword := userAndPassword
    httpApi
  }

  @TestOnly
  def journalActorState: Output.JournalActorState =
    (actorSystem.actorSelection("user/Journal") ? JournalActor.Input.GetJournalActorState)(Timeout(99.s))
      .mapTo[JournalActor.Output.JournalActorState]
      .await(99.s)

  def close() = {
    for (o <- _httpApi) o.close()  // Close before server
    closer.close()
  }
}

object RunningController
{
  private val logger = Logger(getClass)

  def run(configuration: ControllerConfiguration, timeout: Option[FiniteDuration] = None)(body: RunningController => Unit)(implicit s: Scheduler): Unit =
    autoClosing(start(configuration) await timeout) { controller =>
      for (t <- controller.terminated.failed) logger.error(t.toStringWithCauses, t)
      body(controller)
      controller.terminated await timeout
    }

  def newInjectorForTest(directory: Path, module: Module = EMPTY_MODULE,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None,
    name: String,
    scheduler: Option[Scheduler] = None)
  : Injector =
    Guice.createInjector(DEVELOPMENT,
      Modules `override` new ControllerModule(
        ControllerConfiguration.forTest(
          configAndData = directory,
          config,
          httpPort = httpPort,
          httpsPort = httpsPort,
          name = name),
        commonScheduler = scheduler)
      `with` module)

  def blockingRun(conf: ControllerConfiguration)(whileRunning: RunningController => Unit)
  : ProgramTermination =
    autoClosing(RunningController.start(conf).awaitInfinite) { runningController =>
      whileRunning(runningController)
      try runningController.terminated.awaitInfinite
      catch {
        case t: RestartAfterJournalTruncationException =>
          logger.info(t.getMessage)
          ProgramTermination(restart = true)
      }
    }

  def start(configuration: ControllerConfiguration): Future[RunningController] =
    fromInjector(Guice.createInjector(PRODUCTION, new ControllerModule(configuration)))

  def fromInjector(injector: Injector): Future[RunningController] = {
    implicit val scheduler: Scheduler = injector.instance[Scheduler]
    // Run under scheduler from start (and let debugger show Controller's thread names)
    CorrelId.bindNew(
      Future {
        new Starter(injector).start()
      }.flatten)
  }

  private class Starter(injector: Injector)
  {
    private val controllerConfiguration = injector.instance[ControllerConfiguration]
    private implicit val iox = injector.instance[IOExecutor]
    private implicit val scheduler: Scheduler = injector.instance[Scheduler]
    private implicit lazy val closer: Closer = injector.instance[Closer]
    private implicit lazy val actorSystem: ActorSystem = injector.instance[ActorSystem]

    import controllerConfiguration.{implicitAkkaAskTimeout, journalMeta}
    @volatile private var clusterStartupTermination = ProgramTermination()

    private[RunningController] def start(): Future[RunningController] = {
      val whenRecovered = Future {
        // May take several seconds !!!
        StateRecoverer.recover[ControllerState](journalMeta, controllerConfiguration.config)
      }
      val testEventBus = injector.instance[StandardEventBus[Any]]
      val whenReady = Promise[Unit]()
      whenReady.completeWith(
        testEventBus.when[ControllerOrderKeeper.ControllerReadyTestIncident.type].void.runToFuture)
      // Start-up some stuff while recovering

      val directoryWatchingSignatureVerifier =
        DirectoryWatchingSignatureVerifier
          // Do not disturb recovery and ClusterNode start-up log output now
          // and start verifier later
          .checkedResource(
            controllerConfiguration.config,
            onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))
          .orThrow
          .startService
          .awaitInfinite
      closer.onClose {
        // TODO Use a StoppableRegister
        directoryWatchingSignatureVerifier.stop.await(9.s)
      }
      val itemVerifier = new SignedItemVerifier(
        directoryWatchingSignatureVerifier,
        ControllerState.signableItemJsonCodec)

      val recovered = Await.result(whenRecovered, Duration.Inf).closeWithCloser
      val persistence = FileStatePersistence.prepare[ControllerState](
        recovered.journalId, recovered.eventWatch,
        journalMeta, controllerConfiguration.journalConf,
        injector.instance[EventIdGenerator], injector.instance[StampedKeyedEventBus])
      val (clusterNode, controllerState, untilClusterRecovered) =
        startClusterNode(recovered, persistence, testEventBus)

      val clusterRecoveredFuture = untilClusterRecovered.runToFuture

      val (orderKeeperActor, orderKeeperTerminated) = {
        val orderKeeperStarted = clusterRecoveredFuture
          .map(_.flatMap(recovered =>
            startControllerOrderKeeper(
              persistence,
              clusterNode.workingClusterNode.orThrow/*TODO*/,
              recovered,
              testEventBus)))

        val actorTask: Task[Checked[ActorRef @@ ControllerOrderKeeper]] =
          Task.defer {
            controllerState.map(_.map(_.clusterState)).flatMapT { clusterState =>
              import controllerConfiguration.clusterConf.{isBackup, ownId}
              if (!clusterState.isActive(ownId, isBackup = isBackup))
                Task.pure(Left(ClusterNodeIsNotActiveProblem))
              else
                Task.fromFuture(orderKeeperStarted)
                  .map {
                    case Left(_) => Left(ShuttingDownProblem)
                    case Right(o) => Right(o.actor)
                  }
            }
          }

        val terminated = orderKeeperStarted.flatMap {
          case Left(termination) =>
            Future.successful(termination)
          case Right(o) =>
            o.termination andThen { case tried =>
              for (t <- tried.failed) {
                logger.error(s"ControllerOrderKeeper failed with ${t.toStringWithCauses}", t)  // Support diagnosis
              }
              clusterRecoveredFuture.cancel()
            }
        }

        (actorTask, terminated)
      }

      for (t <- orderKeeperActor.failed) logger.debug("orderKeeperActor => " + t.toStringWithCauses, t)
      orderKeeperActor.failed foreach whenReady.tryFailure
      orderKeeperTerminated.failed foreach whenReady.tryFailure
      val commandExecutor = new ControllerCommandExecutor(
        new MyCommandExecutor(
          clusterNode,
          onShutDownBeforeClusterActivated = termination =>
            Task {
              clusterStartupTermination = termination
              clusterRecoveredFuture.cancel()
              Completed
            },
          orderKeeperActor))

      val itemUpdater = new MyItemUpdater(itemVerifier, orderKeeperActor)

      val orderApi = new MainOrderApi(controllerState)

      injector.instance[ControllerWebServer.Factory]
        .resource(orderApi, commandExecutor, itemUpdater,
          controllerState,
          clusterNode,
          recovered.totalRunningSince, // Maybe different from JournalHeader
          recovered.eventWatch)
        .startService
        .flatMap(webServer =>
          Task.defer {
            // TODO Use a StoppableRegister
            closer.onClose(webServer.stop.await(99.s))
            injector
              .instance[SessionRegister[SimpleSession]]
              .placeSessionTokenInDirectoryLegacy(
                SimpleUser.System,
                controllerConfiguration.workDirectory,
                closer)
              .map { _ =>
                controllerConfiguration.workDirectory / "http-uri" :=
                  webServer.localHttpUri.fold(_ => "", o => s"$o/controller")
                new RunningController(recovered.eventWatch.strict, webServer,
                  recoveredEventId = recovered.eventId,
                  orderApi,
                  controllerState.map(_.orThrow),
                  commandExecutor,
                  itemUpdater,
                  whenReady.future, orderKeeperTerminated, testEventBus, closer, injector)
              }
          })
      .runToFuture
    }

    private def startClusterNode(
      recovered: Recovered[ControllerState],
      persistence: FileStatePersistence[ControllerState],
      testEventBus: StandardEventBus[Any])
    : (ClusterNode[ControllerState],
      Task[Checked[ControllerState]],
      Task[Either[ProgramTermination, Recovered[ControllerState]]])
    = {
      val clusterNode = {
        import controllerConfiguration.{clusterConf, config, httpsConfig}
        ClusterNode[ControllerState](
          persistence,
          journalMeta,
          clusterConf,
          config,
          injector.instance[EventIdGenerator],
          (uri, name) => AkkaHttpControllerApi
            .resource(uri, clusterConf.peersUserAndPassword, httpsConfig, name = name),
          new LicenseChecker(LicenseCheckContext(controllerConfiguration.configDirectory)),
          testEventBus,
          implicitAkkaAskTimeout)
      }

      class StartingClusterCancelledException extends NoStackTrace
      // untilClusterNodeRecovered terminates when this cluster node becomes active or terminates
      // replicatedState accesses the current ControllerState while this node is passive, otherwise it is None
      val (replicatedState, untilActiveRecovered_) = clusterNode.start(recovered)
      val untilClusterNodeRecovered = untilActiveRecovered_
        .doOnCancel(Task { logger.debug("ClusterNode canceled") })
        .onCancelRaiseError(new StartingClusterCancelledException)
        .map(_.orThrow)
        .map(Right.apply)
        .onErrorRecoverWith {
          case _: StartingClusterCancelledException => Task { Left(clusterStartupTermination) }
        }
        .flatTap {
          case Right(recovered) =>
            persistence.start(recovered)
          case _ =>
            clusterNode.stop
              .void
              .onErrorHandle(t =>
                logger.error(t.toStringWithCauses, t.nullIfNoStackTrace))
        }
      val controllerState = Task.defer {
        if (persistence.isStarted)
          persistence.awaitCurrentState map Right.apply
        else
          replicatedState.map(_.toChecked(ClusterNodeIsNotReadyProblem).flatten)
      }
      (clusterNode, controllerState, untilClusterNodeRecovered)
    }

    private def startControllerOrderKeeper(
      persistence: FileStatePersistence[ControllerState],
      workingClusterNode: WorkingClusterNode[ControllerState],
      recovered: Recovered[ControllerState],
      testEventPublisher: EventPublisher[Any])
    : Either[ProgramTermination, OrderKeeperStarted] = {
      val terminationPromise = Promise[ProgramTermination]()
      val actor = actorSystem.actorOf(
        Props {
          new ControllerOrderKeeper(terminationPromise, persistence, workingClusterNode,
            injector.instance[AlarmClock],
            controllerConfiguration, testEventPublisher)
        },
        "ControllerOrderKeeper")
      actor ! ControllerOrderKeeper.Input.Start(recovered)
      val termination = terminationPromise.future
        .andThen { case Failure(t) => logger.error(t.toStringWithCauses, t) }
        .andThen { case _ => closer.close() }  // Close automatically after termination
      Right(OrderKeeperStarted(actor.taggedWith[ControllerOrderKeeper], termination))
    }
  }

  private class MyCommandExecutor(
    clusterNode: ClusterNode[ControllerState],
    onShutDownBeforeClusterActivated: ProgramTermination => Task[Completed],
    orderKeeperActor: Task[Checked[ActorRef @@ ControllerOrderKeeper]])
    (implicit timeout: Timeout)
  extends CommandExecutor[ControllerCommand]
  {
    def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]] =
      (command match {
        case command: ControllerCommand.ShutDown =>
          logger.info(s"❗ $command")
          if (command.clusterAction.nonEmpty && !clusterNode.isWorkingNode)
            Task.pure(Left(PassiveClusterNodeShutdownNotAllowedProblem))
          else {
            if (command.dontNotifyActiveNode && clusterNode.isPassive) {
              clusterNode.dontNotifyActiveNodeAboutShutdown()
            }
            onShutDownBeforeClusterActivated(ProgramTermination(restart = command.restart)) >>
              orderKeeperActor.flatMap {
                case Left(ClusterNodeIsNotActiveProblem | ShuttingDownProblem) =>
                  Task.right(ControllerCommand.Response.Accepted)
                case Left(problem) => Task.pure(Left(problem))
                case Right(actor) =>
                  Task.deferFuture(
                    (actor ? ControllerOrderKeeper.Command.Execute(command, meta, CorrelId.current))
                      .mapTo[Checked[ControllerCommand.Response]])
              }
          }

        case ControllerCommand.ClusterAppointNodes(idToUri, activeId) =>
          Task(clusterNode.workingClusterNode)
            .flatMapT(_.appointNodes(idToUri, activeId))
            .map(_.map((_: Completed) => ControllerCommand.Response.Accepted))

        case _ =>
          orderKeeperActor.flatMapT(actor =>
            Task.deferFuture(
              (actor ? ControllerOrderKeeper.Command.Execute(command, meta, CorrelId.current))
                .mapTo[Checked[ControllerCommand.Response]]))
      }).map(_.map((_: ControllerCommand.Response).asInstanceOf[command.Response]))
  }

  private class MyItemUpdater(
    val signedItemVerifier: SignedItemVerifier[SignableItem],
    orderKeeperActor: Task[Checked[ActorRef @@ ControllerOrderKeeper]])
    (implicit timeout: Timeout)
  extends ItemUpdater
  {
    def updateItems(verifiedUpdateItems: VerifiedUpdateItems) =
      orderKeeperActor
        .flatMapT(actor =>
          Task.deferFuture(
            (actor ? ControllerOrderKeeper.Command.VerifiedUpdateItemsCmd(verifiedUpdateItems))
              .mapTo[Checked[Completed]]))
  }

  private case class OrderKeeperStarted(
    actor: ActorRef @@ ControllerOrderKeeper,
    termination: Future[ProgramTermination])

  type ItemSignatureKeysUpdated = ItemSignatureKeysUpdated.type
  case object ItemSignatureKeysUpdated
}
