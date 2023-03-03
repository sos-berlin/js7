package js7.controller

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import cats.effect.{Resource, Sync, SyncIO}
import cats.syntax.traverse.*
import com.google.inject.Stage.PRODUCTION
import com.google.inject.util.Modules
import com.google.inject.{Guice, Injector, Module}
import com.softwaremill.diffx.generic.auto.*
import com.softwaremill.tagging.{@@, Tagger}
import com.typesafe.config.Config
import js7.base.auth.{SimpleUser, UserAndPassword}
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger.syntax.*
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
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SyncResource.syntax.RichSyncResource
import js7.base.utils.{Closer, ProgramTermination, SetOnce}
import js7.cluster.ClusterNode.RestartAfterJournalTruncationException
import js7.cluster.{ClusterNode, WorkingClusterNode}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.Akkas
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.system.ThreadPools
import js7.controller.ControllerOrderKeeper.ControllerIsShuttingDownProblem
import js7.controller.RunningController.*
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.configuration.inject.ControllerModule
import js7.controller.item.ItemUpdater
import js7.controller.web.ControllerWebServer
import js7.core.command.{CommandExecutor, CommandMeta}
import js7.core.license.LicenseChecker
import js7.data.Problems.{ClusterNodeIsNotActiveProblem, PassiveClusterNodeShutdownNotAllowedProblem}
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerCommand.{AddOrder, ShutDown}
import js7.data.controller.{ControllerCommand, ControllerState, VerifiedUpdateItems}
import js7.data.crypt.SignedItemVerifier
import js7.data.event.EventId
import js7.data.item.{ItemOperation, SignableItem, UnsignedSimpleItem}
import js7.data.order.FreshOrder
import js7.journal.JournalActor.Output
import js7.journal.recover.{Recovered, StateRecoverer}
import js7.journal.state.FileStatePersistence
import js7.journal.watch.{JournalEventWatch, StrictEventWatch}
import js7.journal.{EventIdGenerator, JournalActor}
import js7.license.LicenseCheckContext
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

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
  val sessionRegister: SessionRegister[SimpleSession],
  val actorSystem: ActorSystem,
  val testEventBus: StandardEventBus[Any],
  closer: Closer,
  val injector: Injector)
{
  implicit val scheduler: Scheduler = injector.instance[Scheduler]
  val config: Config = injector.instance[Config]
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

  val terminated: Future[ProgramTermination] =
    terminated1
      .map { o =>
        close()
        o
      }

  private def controllerOrderKeeperResource(
    persistence: FileStatePersistence[ControllerState],
    workingClusterNode: WorkingClusterNode[ControllerState],
    recovered: Recovered[ControllerState],
    testEventPublisher: EventPublisher[Any])
  : Resource[Task, OrderKeeperStarted] =
    Resource.make(
      acquire = Task {
        val terminationPromise = Promise[ProgramTermination]()
        val actor = actorSystem.actorOf(
          Props {
            new ControllerOrderKeeper(terminationPromise, persistence, workingClusterNode,
              injector.instance[AlarmClock],
              controllerConfiguration, testEventPublisher)
          },
          "ControllerOrderKeeper")
        actor ! ControllerOrderKeeper.Input.Start(recovered.recoveredState)
        val whenterminated = terminationPromise.future
          .andThen { case Failure(t) => logger.error(t.toStringWithCauses, t) }
          //??? .andThen { case _ => closer.close() } // Close automatically after termination
        OrderKeeperStarted(actor.taggedWith[ControllerOrderKeeper], whenterminated)
      })(
      release = orderKeeperStarted => Task(
        actorSystem.stop(orderKeeperStarted.actor)))

  def terminate(
    suppressSnapshot: Boolean = false,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    dontNotifyActiveNode: Boolean = false)
  : Task[ProgramTermination] =
    Task.defer {
      if (terminated.isCompleted)  // Works only if previous termination has been completed
        Task.fromFuture(terminated)
      else
        actorSystem.whenTerminated.value match {
          case Some(Failure(t)) => Task.raiseError(t)
          case Some(Success(_)) =>
            logger.warn("Controller terminate: Akka has already been terminated")
            Task.pure(ProgramTermination(restart = false))
          case None =>
            logger.debugTask(
              for {
                _ <- _httpApi.toOption.fold(Task.unit)(_
                  .tryLogout.void.onErrorHandle(t => logger.warn(t.toString)))
                _ <-
                  executeCommandAsSystemUser(
                    ControllerCommand.ShutDown(
                      suppressSnapshot = suppressSnapshot,
                      clusterAction = clusterAction,
                      dontNotifyActiveNode = dontNotifyActiveNode)
                  ).flatMap {
                    case Left(problem @ ControllerIsShuttingDownProblem) =>
                      logger.info(problem.toString)
                      Task.fromFuture(terminated).map(Right(_))
                    case o => Task.pure(o)
                  }.map(_.orThrow)
                t <- Task.fromFuture(terminated)
              } yield t)
        }
    }.logWhenItTakesLonger

  def executeCommandAsSystemUser(command: ControllerCommand): Task[Checked[command.Response]] =
    for {
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse(session =>
        executeCommand(command, CommandMeta(session.currentUser)))
    } yield checkedChecked.flatten

  def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    logger.debugTask("executeCommand", command.toShortString)(
      commandExecutor.executeCommand(command, meta)
        .executeOn(scheduler))

  def updateUnsignedSimpleItemsAsSystemUser(items: Seq[UnsignedSimpleItem]): Task[Checked[Completed]] =
    sessionRegister.systemUser
      .flatMapT(updateUnsignedSimpleItems(_, items))
      .executeOn(scheduler)

  def updateUnsignedSimpleItems(user: SimpleUser, items: Seq[UnsignedSimpleItem]): Task[Checked[Completed]] =
    VerifiedUpdateItems
      .fromOperations(
        Observable.fromIterable(items)
          .map(ItemOperation.AddOrChangeSimple.apply),
        _ => Left(Problem.pure("updateUnsignedSimpleItems and verify?")),
        user)
      .flatMapT(itemUpdater.updateItems)
      .executeOn(scheduler)

  def updateItemsAsSystemUser(operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    sessionRegister.systemUser
      .flatMapT(updateItems(_, operations))

  def updateItems(user: SimpleUser, operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    VerifiedUpdateItems
      .fromOperations(operations, itemUpdater.signedItemVerifier.verify, user)
      .flatMapT(itemUpdater.updateItems)
      .executeOn(scheduler)

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
  def journalActorState: Output.JournalActorState = {
    val actorSel = actorSystem.actorSelection("user/Journal")
    // Wait for JournalActor start
    waitForCondition(10.s, 10.ms)(Try(actorSel.resolveOne(99.s).await(99.s)).isSuccess)
    val actor = actorSel.resolveOne(99.s).await(99.s)
    (actor ? JournalActor.Input.GetJournalActorState)(Timeout(99.s))
    .mapTo[JournalActor.Output.JournalActorState]
    .await(99.s)
  }

  def close() =
    logger.debugCall {
      for (o <- _httpApi) o.close()  // Close before server
      closer.close()
    }
}

object RunningController
{
  private val logger = Logger(getClass)

  @TestOnly
  def blockingRun(conf: ControllerConfiguration)(whileRunning: RunningController => Unit)
  : ProgramTermination =
    threadPoolResource[SyncIO](conf).useSync(implicit scheduler =>
      resource(conf, scheduler)
        .use(runningController => Task {
          whileRunning(runningController)
          runningController.terminated.awaitInfinite
        })
        .awaitInfinite)

  def resource(conf: ControllerConfiguration, scheduler: Scheduler, module: Option[Module] = None)
  : Resource[Task, RunningController] = {
    val controllerModule = new ControllerModule(
      conf,
      commonScheduler = Some(scheduler))
    resource(module.fold_(
      controllerModule,
      module => Modules.`override`(controllerModule).`with`(module)))
  }

  private def resource(module: Module): Resource[Task, RunningController] =
    for {
      injector <- injectorResource(module)
      runningController <- resource(injector)
    } yield runningController

  private def injectorResource(module: Module): Resource[Task, Injector] =
    Resource.make(
      acquire = Task(Guice.createInjector(PRODUCTION, module)))(
      release = injector => Task(injector.instance[Closer].close()))

  private def resource(injector: Injector): Resource[Task, RunningController] = {
    implicit val iox = injector.instance[IOExecutor]
    implicit val scheduler = injector.instance[Scheduler]
    lazy val closer = injector.instance[Closer]
    lazy val testEventBus = injector.instance[StandardEventBus[Any]]
    val conf = injector.instance[ControllerConfiguration]

    import conf.{config, implicitAkkaAskTimeout, journalMeta}

    def directoryWatchingSignatureVerifierResource
    : Resource[Task, DirectoryWatchingSignatureVerifier] =
      DirectoryWatchingSignatureVerifier
        .checkedResource(
          config,
          onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))
        .orThrow

    val itemVerifierResource: Resource[Task, SignedItemVerifier[SignableItem]] =
      for {
        directoryWatchingSignatureVerifier <- directoryWatchingSignatureVerifierResource
        itemVerifier <- Resource.eval(Task(new SignedItemVerifier(
          directoryWatchingSignatureVerifier,
          ControllerState.signableItemJsonCodec)))
      } yield itemVerifier

    def clusterNodeResource(
      recovered: Recovered[ControllerState],
      testEventBus: StandardEventBus[Any])(
      implicit actorSystem: ActorSystem)
    : Resource[Task, ClusterNode[ControllerState]] = {
      import conf.{clusterConf, config, httpsConfig, journalConf}
      ClusterNode.resource(
        recovered,
        journalMeta, journalConf, clusterConf, config,
        injector.instance[EventIdGenerator],
        (uri, name) => AkkaHttpControllerApi.resource(
          uri, clusterConf.peersUserAndPassword, httpsConfig, name = name),
        new LicenseChecker(LicenseCheckContext(conf.configDirectory)),
        testEventBus,
        implicitAkkaAskTimeout
      ).orThrow
    }

    def startControllerOrderKeeper(
      persistence: FileStatePersistence[ControllerState],
      workingClusterNode: WorkingClusterNode[ControllerState],
      controllerState: Option[ControllerState],
      testEventPublisher: EventPublisher[Any])(
      implicit actorSystem: ActorSystem)
    : Either[ProgramTermination, OrderKeeperStarted] =
      logger.traceCall {
        val terminationPromise = Promise[ProgramTermination]()
        val actor = actorSystem.actorOf(
          Props {
            new ControllerOrderKeeper(terminationPromise, persistence, workingClusterNode,
              injector.instance[AlarmClock],
              conf, testEventPublisher)
          },
          "ControllerOrderKeeper")
        actor ! ControllerOrderKeeper.Input.Start(controllerState)
        val termination = terminationPromise.future
          .andThen { case Failure(t) => logger.error(t.toStringWithCauses, t) }
          .andThen { case _ => closer.close() } // Close automatically after termination
        Right(OrderKeeperStarted(actor.taggedWith[ControllerOrderKeeper], termination))
      }

    // Recover and initialize in parallel
    val resources =
      StateRecoverer.resource[ControllerState](journalMeta, config)
        .parZip(Akkas.actorSystemResource(conf.name, config))
        .flatMap { case (recovered, actorSystem) =>
          clusterNodeResource(recovered, testEventBus)(actorSystem)
            .map((recovered, actorSystem, _))
        }
        .parZip(itemVerifierResource)

    resources.flatMap { case ((recovered, actorSystem, clusterNode), itemVerifier) =>
      @volatile var clusterStartupTermination = ProgramTermination()
      implicit val implicitActorSystem = actorSystem
      val whenReady = Promise[Unit]()
      whenReady.completeWith(
        testEventBus.when[ControllerOrderKeeper.ControllerReadyTestIncident.type].void.runToFuture)

      val controllerState = clusterNode.currentState
      val orderApi = new MainOrderApi(controllerState)

      val orderKeeperStarted: Task[Either[ProgramTermination, OrderKeeperStarted]] =
        logger.traceTaskWithResult(
          clusterNode.untilActivated
            .map {
              case None =>
                Left(clusterStartupTermination)

              case Some((recovered, workingClusterNode)) =>
                startControllerOrderKeeper(
                  workingClusterNode.persistence,
                  clusterNode.workingClusterNode.orThrow /*TODO*/ ,
                  recovered.recoveredState,
                  testEventBus)
            }
            .onErrorRecover { case t: RestartAfterJournalTruncationException =>
              logger.info(t.getMessage)
              Left(ProgramTermination(restart = true))
            }
        ).memoize

      val orderKeeperActor: Task[Checked[ActorRef @@ ControllerOrderKeeper]] =
        logger.traceTask(
          controllerState
            .map(_.map(_.clusterState))
            .flatMapT { clusterState =>
              import conf.clusterConf.{isBackup, ownId}
              if (!clusterState.isActive(ownId, isBackup = isBackup))
                Task.left(ClusterNodeIsNotActiveProblem)
              else
                orderKeeperStarted
                  .map {
                    case Left(_) => Left(ShuttingDownProblem)
                    case Right(o) => Right(o.actor)
                  }
            })

      val orderKeeperTerminated = logger.traceTask(
        orderKeeperStarted
          .flatMap {
            case Left(termination) => Task.pure(termination)
            case Right(o) =>
              Task.fromFuture(
                o.termination andThen { case tried =>
                  for (t <- tried.failed) { // Support diagnosis
                    logger.error(
                      s"ControllerOrderKeeper failed with ${t.toStringWithCauses}", t)
                  }
                })
          }
          .uncancelable/*a test may use this in `race`, unintentionally canceling this*/)

      val commandExecutor = new ControllerCommandExecutor(
        new MyCommandExecutor(
          clusterNode,
          onShutDownBeforeClusterActivated = termination =>
            Task.defer {
              clusterStartupTermination = termination
              clusterNode.onShutdown.as(Completed)
            },
          orderKeeperActor))

      val itemUpdater = new MyItemUpdater(itemVerifier, orderKeeperActor)

      val sessionRegister: SessionRegister[SimpleSession] =
        SessionRegister.start(actorSystem, SimpleSession.apply, config)

      def webServerResource(eventWatch: JournalEventWatch, totalRunningSince: Deadline)
      : Resource[Task, AkkaWebServer & AkkaWebServer.HasUri] =
        ControllerWebServer
          .resource(orderApi, commandExecutor, itemUpdater,
            controllerState,
            clusterNode,
            totalRunningSince, // Maybe different from JournalHeader
            eventWatch,
            conf,
            sessionRegister, injector)

      def controllerResource(
        eventId: EventId,
        eventWatch: JournalEventWatch,
        whenReady: Task[Unit],
        webServer: AkkaWebServer & AkkaWebServer.HasUri)
      : Resource[Task, RunningController] = {
        Resource.make(
          acquire = Task.defer(
            sessionRegister
              .placeSessionTokenInDirectoryLegacy(
                SimpleUser.System,
                conf.workDirectory,
                closer)
              .map { _ =>
                conf.workDirectory / "http-uri" :=
                  webServer.localHttpUri.fold(_ => "", o => s"$o/controller")
                new RunningController(eventWatch.strict, webServer,
                  recoveredEventId = eventId,
                  orderApi,
                  controllerState.map(_.orThrow),
                  commandExecutor,
                  itemUpdater,
                  whenReady.runToFuture,
                  logger.traceTask("orderKeeperTerminated.runToFuture")(orderKeeperTerminated).runToFuture,
                  sessionRegister,
                  actorSystem,
                  testEventBus, closer, injector)
              }))(
          release = _.terminate().void)
      }

      webServerResource(recovered.eventWatch, recovered.totalRunningSince)
        .flatMap(webServer =>
          controllerResource(
            recovered.eventId, recovered.eventWatch,
            Task.fromFuture(whenReady.future),
            webServer))
      }

    //for (t <- orderKeeperActor.failed) logger.debug("orderKeeperActor => " + t.toStringWithCauses, t)
    //orderKeeperActor.failed foreach whenReady.tryFailure
    //terminated.failed foreach whenReady.tryFailure
  }

  def threadPoolResource[F[_]](conf: ControllerConfiguration, orCommon: Option[Scheduler] = None)
    (implicit F: Sync[F])
  : Resource[F, Scheduler] =
    ThreadPools.standardSchedulerResource[F](conf.name, conf.config, orCommon = orCommon)

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
            .rightAs(ControllerCommand.Response.Accepted)

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
