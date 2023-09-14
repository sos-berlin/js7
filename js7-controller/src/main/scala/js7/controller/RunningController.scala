package js7.controller

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import cats.effect.{Resource, Sync, SyncIO}
import cats.syntax.traverse.*
import com.softwaremill.diffx.generic.auto.*
import com.softwaremill.tagging.{@@, Tagger}
import com.typesafe.config.Config
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.generic.{Completed, SecretString}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.problem.{Checked, Problem}
import js7.base.service.{MainService, Service}
import js7.base.thread.Futures.implicits.*
import js7.base.thread.IOExecutor
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.CatsBlocking.BlockingTaskResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SyncResource.syntax.RichSyncResource
import js7.base.utils.{Allocated, ProgramTermination}
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatchService
import js7.cluster.{ClusterNode, WorkingClusterNode}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.Akkas.actorSystemResource
import js7.common.system.ThreadPools
import js7.controller.RunningController.logger
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.controller.problems.ControllerIsShuttingDownProblem
import js7.controller.web.ControllerWebServer
import js7.core.command.{CommandExecutor, CommandMeta}
import js7.core.license.LicenseChecker
import js7.data.Problems.{ClusterNodeIsNotActiveProblem, PassiveClusterNodeShutdownNotAllowedProblem}
import js7.data.agent.AgentPath
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerCommand.{AddOrder, ShutDown}
import js7.data.controller.{ControllerCommand, ControllerState, VerifiedUpdateItems}
import js7.data.crypt.SignedItemVerifier
import js7.data.event.EventId
import js7.data.item.{ItemOperation, SignableItem, UnsignedSimpleItem}
import js7.data.node.NodeNameToPassword
import js7.data.order.FreshOrder
import js7.journal.JournalActor.Output
import js7.journal.state.FileJournal
import js7.journal.watch.StrictEventWatch
import js7.journal.{EventIdClock, JournalActor}
import js7.license.LicenseCheckContext
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}
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
  val webServer: AkkaWebServer,
  val recoveredEventId: EventId,
  val orderApi: OrderApi,
  val controllerState: Task[ControllerState],
  commandExecutor: ControllerCommandExecutor,
  itemUpdater: ItemUpdater,
  whenReady: Future[Unit],
  val terminated: Future[ProgramTermination],
  val clusterWatchServiceFor: AgentPath => Task[Checked[ClusterWatchService]],
  val sessionRegister: SessionRegister[SimpleSession],
  val conf: ControllerConfiguration,
  val testEventBus: StandardEventBus[Any],
  val actorSystem: ActorSystem)
  (implicit val scheduler: Scheduler)
extends MainService with Service.StoppableByRequest
{
  protected type Termination = ProgramTermination

  @TestOnly lazy val localUri: Uri =
    webServer.localUri

  val untilTerminated =
    Task.fromFuture(terminated)

  protected def start =
    startService(
      untilStopRequested *>
        shutdown(ShutDown()).void)

  def shutdown(cmd: ShutDown): Task[ProgramTermination] =
    Task.defer {
      if (terminated.isCompleted)  // Works only if previous termination has been completed
        untilTerminated
      else
        logger.debugTask(
          executeCommandAsSystemUser(cmd)
            .rightAs(())
            .flatMapLeftCase { case problem @ ControllerIsShuttingDownProblem =>
              logger.info(problem.toString)
              Task.right(())
            }
            .map(_.orThrow)
            .*>(untilTerminated))
    }

  private def executeCommandAsSystemUser(command: ControllerCommand): Task[Checked[command.Response]] =
    for {
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse(session =>
        executeCommand(command, CommandMeta(session.currentUser)))
    } yield checkedChecked.flatten

  private def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    logger.debugTask("executeCommand", command.toShortString)(
      commandExecutor.executeCommand(command, meta)
        .executeOn(scheduler))

  def updateUnsignedSimpleItemsAsSystemUser(items: Seq[UnsignedSimpleItem]): Task[Checked[Completed]] =
    sessionRegister.systemUser
      .flatMapT(updateUnsignedSimpleItems(_, items))
      .executeOn(scheduler)

  private def updateUnsignedSimpleItems(user: SimpleUser, items: Seq[UnsignedSimpleItem]): Task[Checked[Completed]] =
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

  private def updateItems(user: SimpleUser, operations: Observable[ItemOperation]): Task[Checked[Completed]] =
    VerifiedUpdateItems
      .fromOperations(operations, itemUpdater.signedItemVerifier.verify, user)
      .flatMapT(itemUpdater.updateItems)
      .executeOn(scheduler)

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
  def journalActorState: Output.JournalActorState = {
    val actorSel = actorSystem.actorSelection("user/Journal")
    // Wait for JournalActor start
    waitForCondition(10.s, 10.ms)(Try(actorSel.resolveOne(99.s).await(99.s)).isSuccess)
    val actor = actorSel.resolveOne(99.s).await(99.s)
    (actor ? JournalActor.Input.GetJournalActorState)(Timeout(99.s))
    .mapTo[JournalActor.Output.JournalActorState]
    .await(99.s)
  }
}

object RunningController
{
  private val logger = Logger[this.type]

  @TestOnly
  def blockingRun(conf: ControllerConfiguration, timeout: FiniteDuration)
    (whileRunning: RunningController => Unit)
  : ProgramTermination =
    threadPoolResource[SyncIO](conf).useSync(implicit scheduler =>
      resource(conf)
        .blockingUse(timeout) { runningController =>
          whileRunning(runningController)
          runningController.terminated.awaitInfinite
        })

  def resource(conf: ControllerConfiguration, testWiring: TestWiring = TestWiring.empty)
    (implicit scheduler: Scheduler)
  : Resource[Task, RunningController] = {
    val alarmClock: AlarmClock =
      testWiring.alarmClock getOrElse
        AlarmClock(Some(conf.config
          .getDuration("js7.time.clock-setting-check-interval")
          .toFiniteDuration))(scheduler)

    val eventIdClock: EventIdClock =
      testWiring.eventIdClock getOrElse new EventIdClock(alarmClock)

    for {
      iox <- IOExecutor.resource[Task](conf.config, conf.name + " I/O")
      runningController <- resource(conf, alarmClock, eventIdClock)(scheduler, iox)
    } yield runningController
  }.executeOn(scheduler)

  private def resource(
    conf: ControllerConfiguration,
    alarmClock: AlarmClock,
    eventIdClock: EventIdClock)
    (implicit scheduler: Scheduler, iox: IOExecutor)
  : Resource[Task, RunningController] = {
    import conf.{clusterConf, config, httpsConfig, implicitAkkaAskTimeout, journalLocation}

    implicit val testEventBus = new StandardEventBus[Any]

    implicit val nodeNameToPassword: NodeNameToPassword[ControllerState] = {
      val result = Right(config.optionAs[SecretString]("js7.auth.cluster.password"))
      _ => result
    }

    // Recover and initialize other stuff in parallel
    val clusterNodeResource =
      ClusterNode.recoveringResource[ControllerState](
        actorSystemResource(conf.name, config),
        (admission, name, actorSystem) => AkkaHttpControllerApi.resource(
          admission, httpsConfig, name = name)(actorSystem),
        new LicenseChecker(LicenseCheckContext(conf.configDirectory)),
        journalLocation, clusterConf, eventIdClock, testEventBus)

    val resources = CorrelId.bindNew(clusterNodeResource)
      .parZip(CorrelId.bindNew(
        itemVerifierResource(config, testEventBus)))

    resources.flatMap { case (clusterNode, itemVerifier) =>
      import clusterNode.actorSystem

      val orderKeeperStarted: Task[Either[ProgramTermination, OrderKeeperStarted]] =
        logger.traceTaskWithResult(
          clusterNode.untilActivated
            .map(_.flatMap { workingClusterNode =>
              startControllerOrderKeeper(
                workingClusterNode.journalAllocated,
                clusterNode.workingClusterNode.orThrow,
                alarmClock,
                conf, testEventBus)
            })
        ).memoize

      val controllerState = clusterNode.currentState

      val whenReady = Promise[Unit]()
      whenReady.completeWith(
        testEventBus.when[ControllerOrderKeeper.ControllerReadyTestIncident.type].void.runToFuture)

      // The ControllerOrderKeeper if started
      val currentOrderKeeperActor: Task[Checked[ActorRef @@ ControllerOrderKeeper]] =
        logger.traceTask(
          controllerState
            .map(_.map(_.clusterState))
            .flatMapT { clusterState =>
              import conf.clusterConf.{isBackup, ownId}
              if (!clusterState.isActive(ownId, isBackup = isBackup))
                Task.left(ClusterNodeIsNotActiveProblem)
              else
                orderKeeperStarted.map {
                  case Left(_) => Left(ShuttingDownProblem)
                  case Right(o) => Right(o.actor)
                }
            }
            .tapError(t => Task {
              logger.debug(s"currentOrderKeeperActor => ${t.toStringWithCauses}", t)
              whenReady.tryFailure(t)
            }))

      val untilOrderKeeperTerminated = logger.traceTask(
        orderKeeperStarted.flatMap {
          case Left(termination) => Task.pure(termination)
          case Right(o) =>
            Task
              .fromFuture(o.termination)
              .tapError(t => Task(
                logger.error(s"ControllerOrderKeeper failed with ${t.toStringWithCauses}", t)))
          }
          .tapError(t => Task(whenReady.tryFailure(t)))
      ).uncancelable/*a test may use this in `race`, unintentionally canceling this*/
        .memoize

      val commandExecutor = new ControllerCommandExecutor(
        new MyCommandExecutor(clusterNode, currentOrderKeeperActor))

      val orderApi = new MainOrderApi(controllerState)
      val itemUpdater = new MyItemUpdater(itemVerifier, currentOrderKeeperActor)
      import clusterNode.recoveredExtract

      def webServerResource(sessionRegister: SessionRegister[SimpleSession])
      : Resource[Task, ControllerWebServer] =
        for {
          webServer <- ControllerWebServer.resource(
            orderApi, commandExecutor, itemUpdater, clusterNode,
            recoveredExtract.totalRunningSince, // Maybe different from JournalHeader
            recoveredExtract.eventWatch,
            conf, sessionRegister)
          _ <- webServer.restartWhenHttpsChanges
          _ <- Resource.eval(Task(
            conf.workDirectory / "http-uri" :=
              webServer.localHttpUri.fold(_ => "", o => s"$o/controller")))
        } yield webServer

      def clusterWatchServiceFor(agentPath: AgentPath): Task[Checked[ClusterWatchService]] =
        currentOrderKeeperActor
          .flatMapT(actor =>
            Task.deferFuture(
              (actor ? ControllerOrderKeeper.Command.GetClusterWatchService(agentPath))
                .mapTo[Checked[ClusterWatchService]]))

      def runningControllerResource(
        webServer: ControllerWebServer,
        sessionRegister: SessionRegister[SimpleSession])
      : Resource[Task, RunningController] =
        Service.resource(Task(
          new RunningController(
            recoveredExtract.eventWatch.strict,
            webServer,
            recoveredEventId = recoveredExtract.eventId,
            orderApi,
            controllerState.map(_.orThrow),
            commandExecutor, itemUpdater,
            whenReady.future, untilOrderKeeperTerminated.runToFuture,
            clusterWatchServiceFor,
            sessionRegister, conf, testEventBus,
            actorSystem)))

      for {
        sessionRegister <- SessionRegister.resource(SimpleSession.apply, config)
        _ <- sessionRegister.placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)
        webServer <- webServerResource(sessionRegister)
        runningController <- runningControllerResource(webServer, sessionRegister)
      } yield runningController
    }
  }

  def threadPoolResource[F[_]](conf: ControllerConfiguration, orCommon: Option[Scheduler] = None)
    (implicit F: Sync[F])
  : Resource[F, Scheduler] =
    ThreadPools.standardSchedulerResource[F](conf.name, conf.config, orCommon = orCommon)

  private def itemVerifierResource(
    config: Config,
    testEventBus: StandardEventBus[Any])(
    implicit iox: IOExecutor)
  : Resource[Task, SignedItemVerifier[SignableItem]] =
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
    journalAllocated: Allocated[Task, FileJournal[ControllerState]],
    workingClusterNode: WorkingClusterNode[ControllerState],
    alarmClock: AlarmClock,
    conf: ControllerConfiguration,
    testEventPublisher: EventPublisher[Any])(
    implicit scheduler: Scheduler, actorSystem: ActorSystem)
  : Either[ProgramTermination, OrderKeeperStarted] =
    logger.traceCall {
      val terminationPromise = Promise[ProgramTermination]()
      val actor = actorSystem.actorOf(
        Props {
          new ControllerOrderKeeper(terminationPromise, journalAllocated, workingClusterNode,
            alarmClock, conf, testEventPublisher)
        },
        "ControllerOrderKeeper")
      actor ! ControllerOrderKeeper.Input.Start
      val termination = terminationPromise.future
        .andThen { case Failure(t) => logger.error(t.toStringWithCauses, t) }
      Right(OrderKeeperStarted(actor.taggedWith[ControllerOrderKeeper], termination))
    }

  private class MyCommandExecutor(
    clusterNode: ClusterNode[ControllerState],
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
            clusterNode.stopRecovery(ProgramTermination(restart = command.restart)) >>
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

  final case class TestWiring(
    alarmClock: Option[AlarmClock] = None,
    eventIdClock: Option[EventIdClock] = None)
  object TestWiring {
    val empty = TestWiring()
  }
}
