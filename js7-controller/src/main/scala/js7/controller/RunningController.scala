package js7.controller

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{IO, Resource, ResourceIO, Sync, SyncIO}
import cats.syntax.traverse.*
import com.softwaremill.tagging.{@@, Tagger}
import com.typesafe.config.Config
import fs2.Stream
import js7.base.auth.SimpleUser
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.catsutils.{OurIORuntime, OurIORuntimeRegister}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.generic.{Completed, SecretString}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Log4j, Logger}
import js7.base.monixlike.MonixLikeExtensions.{deferFuture, tapError}
import js7.base.problem.Checked.*
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.problem.{Checked, Problem}
import js7.base.service.{MainService, Service}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.time.{AlarmClock, WallClock}
import js7.base.utils.CatsBlocking.BlockingIOResource
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SyncResource.syntax.RichSyncResource
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatchService
import js7.cluster.{ClusterNode, WorkingClusterNode}
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.common.pekkoutils.Pekkos.actorSystemResource
import js7.controller.RunningController.logger
import js7.controller.client.PekkoHttpControllerApi
import js7.controller.command.{ControllerCommandExecutor, IControllerCommandExecutor}
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.controller.problems.ControllerIsShuttingDownProblem
import js7.controller.web.ControllerWebServer
import js7.core.command.CommandMeta
import js7.core.license.LicenseChecker
import js7.data.Problems.{ClusterNodeIsNotActiveProblem, PassiveClusterNodeShutdownNotAllowedProblem}
import js7.data.agent.AgentPath
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerCommand.{AddOrder, ShutDown}
import js7.data.controller.{ControllerCommand, ControllerState, VerifiedUpdateItems}
import js7.data.crypt.SignedItemVerifier
import js7.data.event.{AnyKeyedEvent, EventId, Stamped}
import js7.data.item.{ItemOperation, SignableItem, UnsignedSimpleItem}
import js7.data.node.NodeNameToPassword
import js7.data.order.FreshOrder
import js7.journal.JournalActor.Output
import js7.journal.watch.StrictEventWatch
import js7.journal.{EventIdGenerator, JournalActor}
import js7.license.LicenseCheckContext
import org.apache.pekko.actor.{ActorRef, ActorSystem, Props}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Try}

/**
 * JS7 Controller.
 *
 * Integration test in engine-tests, for example js7.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class RunningController private(
  val eventWatch: StrictEventWatch,
  val webServer: PekkoWebServer,
  val recoveredEventId: EventId,
  val orderApi: OrderApi,
  val controllerState: IO[ControllerState],
  commandExecutor: ControllerCommandExecutor,
  itemUpdater: ItemUpdater,
  whenReady: Future[Unit],
  val terminated: Future[ProgramTermination],
  val clusterWatchServiceFor: AgentPath => IO[Checked[ClusterWatchService]],
  val sessionRegister: SessionRegister[SimpleSession],
  val conf: ControllerConfiguration,
  val testEventBus: StandardEventBus[Any],
  val actorSystem: ActorSystem)
  (implicit val ioRuntime: IORuntime)
extends MainService, Service.StoppableByRequest:

  protected type Termination = ProgramTermination

  @TestOnly lazy val localUri: Uri =
    webServer.localUri

  val untilTerminated: IO[ProgramTermination] =
    IO.fromFutureDummyCancelable(IO.pure(terminated))

  protected def start =
    startService(
      untilStopRequested *>
        shutdown(ShutDown()).void)

  def shutdown(cmd: ShutDown): IO[ProgramTermination] =
    IO.defer:
      if terminated.isCompleted then  // Works only if previous termination has been completed
        untilTerminated
      else
        logger.debugIO(
          executeCommandAsSystemUser(cmd)
            .rightAs(())
            .flatMapLeftCase { case problem @ ControllerIsShuttingDownProblem =>
              logger.info(problem.toString)
              IO.right(())
            }
            .map(_.orThrow)
            .*>(untilTerminated))

  private def executeCommandAsSystemUser(command: ControllerCommand): IO[Checked[command.Response]] =
    for
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse(session =>
        executeCommand(command, CommandMeta(session.currentUser)))
    yield checkedChecked.flatten

  private def executeCommand(command: ControllerCommand, meta: CommandMeta): IO[Checked[command.Response]] =
    logger.debugIO(s"executeCommand ${command.toShortString}")(
      commandExecutor.executeCommand(command, meta))
        .evalOn(ioRuntime.compute)

  def updateUnsignedSimpleItemsAsSystemUser(items: Seq[UnsignedSimpleItem]): IO[Checked[Completed]] =
    sessionRegister.systemUser
      .flatMapT(updateUnsignedSimpleItems(_, items))
      .evalOn(ioRuntime.compute)

  private def updateUnsignedSimpleItems(user: SimpleUser, items: Seq[UnsignedSimpleItem]): IO[Checked[Completed]] =
    VerifiedUpdateItems
      .fromOperations(
        Stream.iterable(items)
          .map(ItemOperation.AddOrChangeSimple.apply),
        _ => Left(Problem.pure("updateUnsignedSimpleItems and verify?")),
        user)
      .flatMapT(itemUpdater.updateItems)
      .evalOn(ioRuntime.compute)

  def updateItemsAsSystemUser(operations: Stream[IO, ItemOperation]): IO[Checked[Completed]] =
    sessionRegister.systemUser
      .flatMapT(updateItems(_, operations))

  private def updateItems(user: SimpleUser, operations: Stream[IO, ItemOperation]): IO[Checked[Completed]] =
    VerifiedUpdateItems
      .fromOperations(operations, itemUpdater.signedItemVerifier.verify, user)
      .flatMapT(itemUpdater.updateItems)
      .evalOn(ioRuntime.compute)

  @TestOnly
  def addOrder(order: FreshOrder): IO[Checked[Unit]] =
    executeCommandAsSystemUser(AddOrder(order))
      .mapT(response =>
        (!response.ignoredBecauseDuplicate) !! Problem(s"Duplicate OrderId '${order.id}'"))

  @TestOnly
  def waitUntilReady()(using sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line): Unit =
    untilReady.await(99.s)

  def untilReady: IO[Unit] =
    IO.fromFutureDummyCancelable(IO(whenReady))

  @TestOnly
  def clusterState: IO[ClusterState] =
    controllerState.map(_.clusterState)

  @TestOnly
  def journalActorState: Output.JournalActorState =
    val actorSel = actorSystem.actorSelection("user/Journal")
    // Wait for JournalActor start
    waitForCondition(10.s, 10.ms)(Try(actorSel.resolveOne(99.s).await(99.s)).isSuccess)
    val actor = actorSel.resolveOne(99.s).await(99.s)
    (actor ? JournalActor.Input.GetJournalActorState)(Timeout(99.s))
    .mapTo[JournalActor.Output.JournalActorState]
    .await(99.s)


object RunningController:
  private lazy val logger = Logger[this.type]

  @TestOnly
  def blockingRun(conf: ControllerConfiguration, timeout: FiniteDuration)
    (whileRunning: RunningController => Unit)
  : ProgramTermination =
    ioRuntimeResource[SyncIO](conf).useSync(implicit ioRuntime =>
      resource(conf)
        .blockingUse(timeout): runningController =>
          whileRunning(runningController)
          runningController.terminated.awaitInfinite)

  def resource(conf: ControllerConfiguration, testWiring: TestWiring = TestWiring.empty)
    (using ioRuntime: IORuntime)
  : ResourceIO[RunningController] =
    Resource.defer:
      Log4j.set("js7.serverId", conf.controllerId.toString)

      given Scheduler = ioRuntime.scheduler
      val alarmClock: AlarmClock =
        testWiring.alarmClock getOrElse
          AlarmClock(Some(conf.config
            .getDuration("js7.time.clock-setting-check-interval")
            .toFiniteDuration))

      val eventIdGenerator: EventIdGenerator =
        testWiring.eventIdGenerator getOrElse EventIdGenerator(alarmClock)

      val testEventBus: StandardEventBus[Any] = new StandardEventBus[Any]
      val env = OurIORuntimeRegister.toEnvironment(ioRuntime)
      for
        _ <- env.registerPure[IO, AlarmClock](alarmClock, ignoreDuplicate = true)
        _ <- env.registerPure[IO, WallClock](alarmClock, ignoreDuplicate = true)
        _ <- env.registerPure[IO, EventPublisher[Stamped[AnyKeyedEvent]]](
          testEventBus.narrowPublisher[Stamped[AnyKeyedEvent]],
          ignoreDuplicate = true)
        _ <- env.registerPure[IO, EventIdGenerator](eventIdGenerator, ignoreDuplicate = true)
        r <- resource2(conf, eventIdGenerator, alarmClock, testEventBus)
      yield r
    .evalOn(ioRuntime.compute)

  private def resource2(
    conf: ControllerConfiguration,
    eventIdGenerator: EventIdGenerator,
    alarmClock: AlarmClock,
    testEventBus: StandardEventBus[Any])
    (implicit ioRuntime: IORuntime)
  : ResourceIO[RunningController] =
    import conf.{clusterConf, config, httpsConfig, implicitPekkoAskTimeout, journalLocation}

    given ExecutionContext = ioRuntime.compute
    given StandardEventBus[Any] = testEventBus

    implicit val nodeNameToPassword: NodeNameToPassword[ControllerState] =
      val result = Right(config.optionAs[SecretString]("js7.auth.cluster.password"))
      _ => result

    // Recover and initialize other stuff in parallel
    val clusterNodeResource =
      ClusterNode.recoveringResource[ControllerState](
        actorSystemResource(conf.name, config),
        (admission, name, actorSystem) => PekkoHttpControllerApi.resource(
          admission, httpsConfig, name = name)(actorSystem),
        new LicenseChecker(LicenseCheckContext(conf.configDirectory)),
        journalLocation, clusterConf, eventIdGenerator, testEventBus)

    val resources =
      Resource.both(
        /*CorrelId.bindNew*/ (clusterNodeResource),
        CorrelId.bindNew:
          itemVerifierResource(config, testEventBus))

    resources.flatMap { (clusterNode, itemVerifier) =>
      import clusterNode.actorSystem

      val orderKeeperStarted: IO[Either[ProgramTermination, OrderKeeperStarted]] =
        memoize:
          logger.traceIOWithResult:
            clusterNode.untilActivated
              .map(_.flatMap { workingClusterNode =>
                startControllerOrderKeeper(
                  clusterNode.workingClusterNode.orThrow,
                  alarmClock,
                  conf, testEventBus)
              })

      val controllerState = clusterNode.currentState

      val whenReady = Promise[Unit]()
      whenReady.completeWith(
        testEventBus.when[ControllerOrderKeeper.ControllerReadyTestIncident.type]
          .map(_ => ()))

      // The ControllerOrderKeeper if started
      val currentOrderKeeperActor: IO[Checked[ActorRef @@ ControllerOrderKeeper]] =
        logger.traceIO(
          controllerState
            .map(_.map(_.clusterState))
            .flatMapT { clusterState =>
              import conf.clusterConf.{isBackup, ownId}
              if !clusterState.isActive(ownId, isBackup = isBackup) then
                IO.left(ClusterNodeIsNotActiveProblem)
              else
                orderKeeperStarted.map {
                  case Left(_) => Left(ShuttingDownProblem)
                  case Right(o) => Right(o.actor)
                }
            }
            .tapError(t => IO {
              logger.debug(s"currentOrderKeeperActor => ${t.toStringWithCauses}", t)
              whenReady.tryFailure(t)
            }))

      val untilOrderKeeperTerminated =
        memoize:
          logger.traceIO(
            orderKeeperStarted.flatMap {
              case Left(termination) => IO.pure(termination)
              case Right(o) =>
                IO
                  .fromFuture(IO.pure(o.termination))
                  .tapError(t => IO(
                    logger.error(s"ControllerOrderKeeper failed with ${t.toStringWithCauses}", t)))
              }
              .tapError(t => IO(whenReady.tryFailure(t)))
          ).uncancelable/*a test may use this in `race`, unintentionally canceling this*/

      val commandExecutor = new ControllerCommandExecutor(
        new MyCommandExecutor(clusterNode, currentOrderKeeperActor))

      val orderApi = new MainOrderApi(controllerState)
      val itemUpdater = new MyItemUpdater(itemVerifier, currentOrderKeeperActor)
      import clusterNode.recoveredExtract

      def webServerResource(sessionRegister: SessionRegister[SimpleSession])
      : ResourceIO[ControllerWebServer] =
        for
          webServer <- ControllerWebServer.resource(
            orderApi, commandExecutor, itemUpdater, clusterNode,
            recoveredExtract.totalRunningSince, // Maybe different from JournalHeader
            recoveredExtract.eventWatch,
            conf, sessionRegister)
          _ <- webServer.restartWhenHttpsChanges
          _ <- Resource.eval(IO(
            conf.workDirectory / "http-uri" :=
              webServer.localHttpUri.fold(_ => "", o => s"$o/controller")))
        yield webServer

      def clusterWatchServiceFor(agentPath: AgentPath): IO[Checked[ClusterWatchService]] =
        currentOrderKeeperActor
          .flatMapT(actor =>
            IO.deferFuture(
              (actor ? ControllerOrderKeeper.Command.GetClusterWatchService(agentPath))
                .mapTo[Checked[ClusterWatchService]]))

      def runningControllerResource(
        webServer: ControllerWebServer,
        sessionRegister: SessionRegister[SimpleSession])
      : ResourceIO[RunningController] =
        Service.resource(IO(
          new RunningController(
            recoveredExtract.eventWatch.strict,
            webServer,
            recoveredEventId = recoveredExtract.eventId,
            orderApi,
            controllerState.map(_.orThrow),
            commandExecutor, itemUpdater,
            whenReady.future, untilOrderKeeperTerminated.unsafeToFuture(),
            clusterWatchServiceFor,
            sessionRegister, conf, testEventBus,
            actorSystem)))

      for
        sessionRegister <- SessionRegister.resource(SimpleSession.apply, config)
        _ <- sessionRegister.placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)
        webServer <- webServerResource(sessionRegister)
        // Stop Journal before before web server to allow receiving acknowledges
        // TODO Start web server before Journal?
        _ <- Resource.onFinalize(clusterNode.workingClusterNode.fold(_ => IO.unit,
          _.journal.stop))
        runningController <- runningControllerResource(webServer, sessionRegister)
      yield
        runningController
    }

  def ioRuntimeResource[F[_]](conf: ControllerConfiguration)(implicit F: Sync[F])
  : Resource[F, IORuntime] =
    OurIORuntime.resource[F](conf.name, conf.config)

  private def itemVerifierResource(
    config: Config,
    testEventBus: StandardEventBus[Any])
  : ResourceIO[SignedItemVerifier[SignableItem]] =
    DirectoryWatchingSignatureVerifier
      .checkedResource(
        config,
        onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))
      .orThrow
      .map(directoryWatchingSignatureVerifier =>
        new SignedItemVerifier(
          directoryWatchingSignatureVerifier,
          ControllerState.signableItemJsonCodec))

  private def startControllerOrderKeeper(
    workingClusterNode: WorkingClusterNode[ControllerState],
    alarmClock: AlarmClock,
    conf: ControllerConfiguration,
    testEventPublisher: EventPublisher[Any])(
    implicit ioRuntime: IORuntime, actorSystem: ActorSystem)
  : Either[ProgramTermination, OrderKeeperStarted] =
    logger.traceCall:
      val terminationPromise = Promise[ProgramTermination]()
      val actor = actorSystem.actorOf(
        Props {
          new ControllerOrderKeeper(terminationPromise, workingClusterNode,
            alarmClock, conf, testEventPublisher)
        },
        "ControllerOrderKeeper")
      actor ! ControllerOrderKeeper.Input.Start
      given ExecutionContext = ioRuntime.compute
      val termination = terminationPromise.future
        .andThen { case Failure(t) => logger.error(t.toStringWithCauses, t) }
      Right(OrderKeeperStarted(actor.taggedWith[ControllerOrderKeeper], termination))

  private class MyCommandExecutor(
    clusterNode: ClusterNode[ControllerState],
    orderKeeperActor: IO[Checked[ActorRef @@ ControllerOrderKeeper]])
    (implicit timeout: Timeout)
  extends IControllerCommandExecutor:
    def executeCommand(command: ControllerCommand, meta: CommandMeta): IO[Checked[command.Response]] =
      command.match
        case command: ControllerCommand.ShutDown =>
          logger.info(s"❗ $command")
          if command.clusterAction.nonEmpty && !clusterNode.isWorkingNode then
            IO.pure(Left(PassiveClusterNodeShutdownNotAllowedProblem))
          else
            clusterNode.onShutdown(
              ProgramTermination(restart = command.restart),
              dontNotifyActiveNode = command.dontNotifyActiveNode && clusterNode.isPassive
            ) >>
              orderKeeperActor.flatMap:
                case Left(ClusterNodeIsNotActiveProblem | ShuttingDownProblem) =>
                  IO.right(ControllerCommand.Response.Accepted)
                case Left(problem) => IO.pure(Left(problem))
                case Right(actor) =>
                  IO.deferFuture(
                    (actor ? ControllerOrderKeeper.Command.Execute(command, meta, CorrelId.current))
                      .mapTo[Checked[ControllerCommand.Response]])

        case ControllerCommand.ClusterAppointNodes(idToUri, activeId) =>
          IO(clusterNode.workingClusterNode)
            .flatMapT(_.appointNodes(idToUri, activeId))
            .rightAs(ControllerCommand.Response.Accepted)

        case _ =>
          orderKeeperActor.flatMapT(actor =>
            IO.deferFuture(
              (actor ? ControllerOrderKeeper.Command.Execute(command, meta, CorrelId.current))
                .mapTo[Checked[ControllerCommand.Response]]))
      .map(_.map((_: ControllerCommand.Response).asInstanceOf[command.Response]))

  private class MyItemUpdater(
    val signedItemVerifier: SignedItemVerifier[SignableItem],
    orderKeeperActor: IO[Checked[ActorRef @@ ControllerOrderKeeper]])
    (implicit timeout: Timeout)
  extends ItemUpdater:
    def updateItems(verifiedUpdateItems: VerifiedUpdateItems) =
      orderKeeperActor
        .flatMapT(actor =>
          IO.deferFuture(
            (actor ? ControllerOrderKeeper.Command.VerifiedUpdateItemsCmd(verifiedUpdateItems))
              .mapTo[Checked[Completed]]))

  private case class OrderKeeperStarted(
    actor: ActorRef @@ ControllerOrderKeeper,
    termination: Future[ProgramTermination])

  type ItemSignatureKeysUpdated = ItemSignatureKeysUpdated.type
  case object ItemSignatureKeysUpdated

  final case class TestWiring(
    alarmClock: Option[AlarmClock] = None,
    eventIdGenerator: Option[EventIdGenerator] = None)
  object TestWiring:
    val empty: TestWiring = TestWiring()
