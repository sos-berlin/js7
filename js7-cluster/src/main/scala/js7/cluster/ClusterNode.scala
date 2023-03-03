package js7.cluster

import akka.actor.ActorRefFactory
import akka.util.Timeout
import cats.effect.{ExitCase, Resource}
import cats.effect.concurrent.Deferred
import cats.syntax.flatMap.*
import com.softwaremill.diffx
import com.typesafe.config.Config
import izumi.reflect.Tag
import java.nio.file.Path
import js7.base.eventbus.EventPublisher
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.service.Service.Started
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.cluster.ClusterNode.*
import js7.cluster.JournalTruncator.truncateJournal
import js7.core.license.LicenseChecker
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotActiveProblem, ClusterNodeIsNotBackupProblem, ClusterNodeIsNotReadyProblem, PrimaryClusterNodeMayNotBecomeBackupProblem}
import js7.data.cluster.ClusterCommand.{ClusterInhibitActivation, ClusterStartBackupNode}
import js7.data.cluster.ClusterState.{Coupled, Empty, FailedOver, HasNodes}
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterCommand, ClusterNodeApi, ClusterSetting, ClusterWatchRequest, ClusterWatchingCommand}
import js7.data.event.{AnyKeyedEvent, EventId, JournalPosition, SnapshotableState, Stamped}
import js7.journal.EventIdGenerator
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalMeta
import js7.journal.recover.{Recovered, StateRecoverer}
import js7.journal.state.FileStatePersistence
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import scala.concurrent.Promise
import scala.util.control.NoStackTrace
import scala.util.{Success, Try}

final class ClusterNode[S <: SnapshotableState[S]: diffx.Diff: Tag] private(
  prepared: Prepared[S],
  passiveOrWorkingNode: AtomicAny[Option[Either[PassiveClusterNode[S], WorkingClusterNode[S]]]],
  journalConf: JournalConf,
  clusterConf: ClusterConf,
  eventIdGenerator: EventIdGenerator,
  eventBus: EventPublisher[Stamped[AnyKeyedEvent]],
  common: ClusterCommon)
  (implicit S: SnapshotableState.Companion[S],
    scheduler: Scheduler,
    actorRefFactory: ActorRefFactory, timeout: akka.util.Timeout)
extends Service.StoppableByRequest
{
  clusterNode =>

  import clusterConf.ownId
  import common.activationInhibitor

  private val workingNodeStarted = Deferred.unsafe[Task, Try[Option[(Recovered[S]/*FIXME Release memory*/, WorkingClusterNode[S])]]]
  private var _testDontNotifyActiveNodeAboutShutdown = false // for test only
  private val recoveryStopped = Deferred.unsafe[Task, Unit]

  @volatile private var _currentState: Task[Either[Problem, S]] =
    prepared.currentPassiveReplicatedState.map(_.toChecked(ClusterNodeIsNotReadyProblem).flatten)

  def dontNotifyActiveNodeAboutShutdown(): Unit =
    _testDontNotifyActiveNodeAboutShutdown = true

  def currentState: Task[Either[Problem, S]] =
    Task.defer(_currentState)

  /** None when stopped before activated. */
  def untilActivated: Task[Option[(Recovered[S], WorkingClusterNode[S])]] =
    logger.traceTaskWithResult("untilActivated", task =
      workingNodeStarted.get.dematerialize)

  /**
   * Returns a pair of `Task`s
   * - `(Task[None], _)` no replicated state available, because it's an active node or the backup node has not yet been appointed.
   * - `(Task[Some[Checked[S]]], _)` with the current replicated state if this node has started as a passive node.
   * - `(_, Task[Checked[ClusterFollowUp]]` when this node should be activated
   *
   * @return A pair of `Task`s with maybe the current `S` of this passive node (if so)
   *         and ClusterFollowUp.
   */
  protected def start: Task[Started] = {
    object StartingClusterCancelledException extends Throwable with NoStackTrace
    // untilClusterNodeRecovered terminates when this cluster node becomes active or terminates
    // replicatedState accesses the current ControllerState while this node is passive, otherwise it is None
    startService(
      logger
        .traceTask("untilActiveNodeRecovered")(
          prepared.untilActiveNodeRecovered)
        .flatMapT(recovered =>
          startWorkingNode(recovered).map(_.map(recovered -> _)))
        .map(_.orThrow)
        .materialize
        .flatTap(triedWorkingNode =>
          workingNodeStarted.complete(triedWorkingNode.map(Some(_))).attempt)
        .dematerialize
        .flatMap { case (recovered, workingNode) =>
          Task {
            _currentState = workingNode.persistence.state.map(Right(_))
          }
        }
        .guaranteeCase {
          case ExitCase.Completed => Task.unit
          // Duplicate ???
          case ExitCase.Error(t) => Task(logger.error(t.toStringWithCauses, t.nullIfNoStackTrace))
          case ExitCase.Canceled => Task(logger.warn("❌ Canceled"))
        }
        .start
        .flatMap(fiber => logger.traceTask("fiber.join")(
          Task.race(recoveryStopped.get, fiber.join)))
        .*>(untilStopRequested)
        .*>(stopRecovery)
        .*>(Task.defer(passiveOrWorkingNode.get() match {
            case Some(Left(passiveClusterNode)) =>
              Task.unless(_testDontNotifyActiveNodeAboutShutdown)(
                passiveClusterNode.notifyActiveNodeAboutShutdown)

            case _ => Task.unit
          }))
        .guarantee(Task.defer(
          passiveOrWorkingNode.get() match {
            case Some(Right(workingClusterNode)) => workingClusterNode.stop
            case _ => Task.unit
          }))
        .guarantee(common.stop))
  }

  def onShutdown: Task[Unit] =
    stopRecovery

  private def stopRecovery: Task[Unit] =
    Task.defer {
      logger.trace("stopRecovery")
      recoveryStopped.complete(()).attempt *>
        workingNodeStarted.complete(Success(None)).attempt.void
    }

  private def startWorkingNode(recovered: Recovered[S]): Task[Checked[WorkingClusterNode[S]]] =
    logger.traceTask(
      FileStatePersistence
        .start[S](
          recovered, journalConf, eventIdGenerator, eventBus)
        .flatMap(persistence => Task.defer {
          val workingClusterNode = new WorkingClusterNode(persistence, common, clusterConf)
          assertThat(!passiveOrWorkingNode.get().exists(_.isRight))
          passiveOrWorkingNode := Some(Right(workingClusterNode))

          workingClusterNode.start(recovered.clusterState, recovered.eventId)
            .rightAs(workingClusterNode)
          /*FIXME left: stop persistence / in stop? / use Resource?*/
        }))

  def workingClusterNode: Checked[WorkingClusterNode[S]] =
    passiveOrWorkingNode.get()
      .flatMap(_.toOption)
      .toRight(ClusterNodeIsNotActiveProblem)

  def executeCommand(command: ClusterCommand): Task[Checked[ClusterCommand.Response]] =
    command match {
      case command: ClusterCommand.ClusterStartBackupNode =>
        Task {
          prepared.expectingStartBackupCommand match {
            case None =>
              if (!clusterConf.isBackup)
                Left(ClusterNodeIsNotBackupProblem)
              else
                Left(Problem.pure("Cluster node is not ready to accept a backup node configuration"))
            case Some(promise) =>
              if (command.setting.passiveId != ownId)
                Left(Problem.pure(s"$command sent to wrong $ownId"))
              else if (command.setting.activeId == ownId)
                Left(Problem.pure(s"$command must not be sent to the active node"))
              else {
                promise.trySuccess(command)
                Right(ClusterCommand.Response.Accepted)
              }
          }
        }

      case ClusterCommand.ClusterInhibitActivation(duration) =>
        activationInhibitor.inhibitActivation(duration)
          .flatMapT(inhibited =>
            if (inhibited)
              Task.pure(Right(ClusterInhibitActivation.Response(None)))
            else {
              workingNodeStarted.get
                .dematerialize
                .flatMap {
                  case None => Task.none
                  case Some((recovered, workingNode)) =>
                    workingNode.persistence.clusterState.map(Some(_))
                }
                .timeoutTo(duration/*???*/ - 500.ms, Task.none)
                .flatMap {
                  case None =>
                    // No persistence
                    Task.left(Problem.pure(
                      "ClusterInhibitActivation command failed — please try again"))

                  case Some(failedOver: FailedOver) =>
                    logger.debug(s"inhibitActivation(${duration.pretty}) => $failedOver")
                    Task.right(ClusterInhibitActivation.Response(Some(failedOver)))

                  case Some(clusterState) =>
                    Task.left(Problem.pure("ClusterInhibitActivation command failed " +
                      s"because node is already active but not failed-over: $clusterState"))
                }
            })

      case _: ClusterCommand.ClusterPrepareCoupling |
           _: ClusterCommand.ClusterCouple |
           _: ClusterCommand.ClusterRecouple |
           _: ClusterCommand.ClusterPassiveDown =>

        if (_testDontNotifyActiveNodeAboutShutdown)
          // Avoid immediate recoupling
          Task.left(Problem(
            s"${command.getClass.simpleScalaName} command rejected due to dontNotifyActiveNode"))
        else
          Task.pure(workingClusterNode)
            .flatMapT(_.executeCommand(command))
    }

  def executeClusterWatchingCommand(command: ClusterWatchingCommand): Task[Checked[Unit]] =
    command match {
      case cmd: ClusterWatchConfirm =>
        Task(passiveOrWorkingNode.get())
          .flatMap {
            case Some(Right(workingClusterNode)) =>
              workingClusterNode.executeClusterWatchConfirm(cmd)
            case _ =>
              common.clusterWatchCounterpart.executeClusterWatchConfirm(cmd)
          }
          .tapEval(result => Task(
            common.testEventPublisher.publish(ClusterWatchConfirmed(cmd, result))))
    }

  def clusterWatchRequestStream: Task[fs2.Stream[Task, ClusterWatchRequest]] =
    common.clusterWatchCounterpart.newStream

  /** Is the active or non-cluster (Empty, isPrimary) node or is becoming active. */
  def isWorkingNode = passiveOrWorkingNode.get().exists(_.isRight)

  def isPassive = passiveOrWorkingNode.get().exists(_.isLeft)

  override def toString = s"ClusterNode(${ownId.string})"
}

object ClusterNode
{
  private val logger = Logger(getClass)

  def resource[S <: SnapshotableState[S] : diffx.Diff : Tag](
    recovered: Recovered[S],
    journalMeta: JournalMeta,
    journalConf: JournalConf,
    clusterConf: ClusterConf,
    config: Config,
    eventIdGenerator: EventIdGenerator,
    clusterNodeApi: (Uri, String) => Resource[Task, ClusterNodeApi],
    licenseChecker: LicenseChecker,
    eventBus: EventPublisher[Any],
    journalActorAskTimeout: Timeout)
    (implicit
      S: SnapshotableState.Companion[S],
      scheduler: Scheduler,
      actorRefFactory: ActorRefFactory,
      timeout: akka.util.Timeout)
  : Checked[Resource[Task, ClusterNode[S]]] = {
    import clusterConf.ownId

    if (recovered.clusterState != Empty) logger.info(
      s"This is cluster $ownId, recovered ClusterState is ${recovered.clusterState}")

    val keepTruncatedRest = config.getBoolean("js7.journal.cluster.keep-truncated-rest")
    val passiveOrWorkingNode =
      AtomicAny[Option[Either[PassiveClusterNode[S], WorkingClusterNode[S]]]](None)
    val common = new ClusterCommon(clusterConf, clusterNodeApi, clusterConf.timing,
      config, licenseChecker, eventBus, journalActorAskTimeout)

    import common.activationInhibitor

    /**
     * Returns a pair of `Task`s
     * - `(Task[None], _)` no replicated state available, because it's an active node or the backup node has not yet been appointed.
     * - `(Task[Some[Checked[S]]], _)` with the current replicated state if this node has started as a passive node.
     * - `(_, Task[Checked[ClusterFollowUp]]` when this node should be activated
     *
     * @return A pair of `Task`s with maybe the current `S` of this passive node (if so)
     *         and ClusterFollowUp.
     */
    def prepare(): Checked[Prepared[S]] =
      recovered.clusterState match {
        case Empty =>
          if (clusterConf.isPrimary) {
            logger.debug(s"Active primary cluster $ownId, no backup node appointed")
            Right(Prepared(
              currentPassiveReplicatedState = Task.none,
              untilActiveNodeRecovered = activationInhibitor.startActive.as(Right(recovered))))
          } else if (recovered.eventId != EventId.BeforeFirst)
            Left(PrimaryClusterNodeMayNotBecomeBackupProblem)
          else
            Right(startAsBackupNodeWithEmptyClusterState())

        case clusterState: HasNodes =>
          if (ownId == clusterState.activeId)
            Right(startAsActiveNodeWithBackup())
          else if (ownId == clusterState.passiveId)
            Right(startAsPassiveNode(recovered, clusterState))
          else
            Left(Problem.pure(
              s"Own cluster $ownId does not match clusterState=${recovered.clusterState}"))
      }

    def startAsBackupNodeWithEmptyClusterState(): Prepared[S] = {
      logger.info(s"Backup cluster $ownId, awaiting appointment from a primary node")
      val startedPromise = Promise[ClusterStartBackupNode]()
      val passiveClusterNode = Task.fromFuture(startedPromise.future)
        .map(cmd =>
          newPassiveClusterNode(recovered, cmd.setting, initialFileEventId = Some(cmd.fileEventId)))
        .memoize
      val currentPassiveState = Task.defer {
        if (startedPromise.future.isCompleted)
          passiveClusterNode.flatMap(_.state.map(s => Some(Right(s))))
        else
          Task.some(Left(BackupClusterNodeNotAppointed))
      }
      val untilActiveRecovered = passiveClusterNode.flatMap(passive =>
        activationInhibitor.startPassive *>
          passive.run(recovered.state))

      Prepared(
        currentPassiveReplicatedState = currentPassiveState,
        untilActiveNodeRecovered = untilActiveRecovered,
        expectingStartBackupCommand = Some(startedPromise))
    }

    def startAsActiveNodeWithBackup(): Prepared[S] =
      recovered.clusterState match {
        case recoveredClusterState: Coupled =>
          import recoveredClusterState.passiveId
          logger.info(s"This cluster $ownId was active and coupled before restart - " +
            s"asking $passiveId about its state")

          val failedOver = common.inhibitActivationOfPeer(recoveredClusterState).map {
            case None /*Other node has not failed-over*/ =>
              logger.info(s"The other $passiveId is up and still passive, " +
                "so this node remains the active cluster node")
              None

            case Some(otherFailedOver) =>
              Some(startPassiveAfterFailover(recoveredClusterState, otherFailedOver))
          }.memoize

          Prepared(
            currentPassiveReplicatedState =
              failedOver.flatMap {
                case None => Task.none
                case Some((_, passiveClusterNode)) => passiveClusterNode.state.map(s => Some(Right(s)))
              },
            untilActiveNodeRecovered =
              failedOver.flatMap {
                case None => Task.right(recovered)
                case Some((ourRecovered, passiveClusterNode)) =>
                  passiveClusterNode.run(ourRecovered.state)
              })

        case _ =>
          logger.info("Remaining the active cluster node, not coupled with passive node")
          Prepared(
            currentPassiveReplicatedState = Task.none,
            untilActiveNodeRecovered = activationInhibitor.startActive.as(Right(recovered)))
      }

    def startPassiveAfterFailover(coupled: Coupled, otherFailedOver: FailedOver)
    : (Recovered[S], PassiveClusterNode[S]) = {
      logger.warn(s"The other ${otherFailedOver.activeId} ${otherFailedOver.toShortString}" +
        ", and became active while this node was absent")
      assertThat(otherFailedOver.idToUri == coupled.idToUri &&
        otherFailedOver.activeId == coupled.passiveId)
      // This restarted, previously failed active cluster node may have written one chunk of events more than
      // the passive node, maybe even an extra snapshot in a new journal file.
      // These extra events are not acknowledged. So we truncate our journal.
      val ourRecovered = truncateJournalAndRecoverAgain(otherFailedOver) match {
        case None => recovered
        case Some(truncatedRecovered) =>
          assertThat(truncatedRecovered.state.clusterState == coupled)
          assertThat(!recovered.eventWatch.whenStarted.isCompleted)
          recovered.close() // Should do nothing, because recovered.eventWatch has not been started
          truncatedRecovered
      }
      ourRecovered -> newPassiveClusterNode(ourRecovered, otherFailedOver.setting, otherFailedOver = true)
    }

    def truncateJournalAndRecoverAgain(otherFailedOver: FailedOver): Option[Recovered[S]] =
      for (file <- truncateJournal(journalMeta.fileBase, otherFailedOver.failedAt, keepTruncatedRest))
        yield recoverFromTruncated(file, otherFailedOver.failedAt)

    def recoverFromTruncated(file: Path, failedAt: JournalPosition): Recovered[S] = {
      logger.info("Recovering again after unacknowledged events have been deleted properly from journal file")
      throw new RestartAfterJournalTruncationException

      // May take a long time !!!
      val recovered = StateRecoverer.recover[S](journalMeta, config)

      // Assertions
      val recoveredJournalFile = recovered.recoveredJournalFile
        .getOrElse(sys.error(s"Unrecoverable journal file: ${file.getFileName}"))
      assertThat(recoveredJournalFile.file == file)
      assertThat(recoveredJournalFile.journalPosition == failedAt,
        s"${recoveredJournalFile.journalPosition} != $failedAt")

      recovered
    }

    def startAsPassiveNode(recovered: Recovered[S], clusterState: HasNodes) = {
      logger.info(
        if (clusterState.isInstanceOf[Coupled])
          s"Remaining a passive cluster node following the active ${clusterState.activeId}"
        else
          s"Remaining a passive cluster node trying to follow the active ${clusterState.activeId}")
      val passive = newPassiveClusterNode(recovered, clusterState.setting)
      Prepared(
        currentPassiveReplicatedState = passive.state.map(s => Some(Right(s))),
        untilActiveNodeRecovered = passive.run(recovered.state))
    }

    def newPassiveClusterNode(
      recovered: Recovered[S],
      setting: ClusterSetting,
      otherFailedOver: Boolean = false,
      initialFileEventId: Option[EventId] = None)
    : PassiveClusterNode[S] = {
      assertThat(!passiveOrWorkingNode.get().exists(_.isLeft))
      val node = new PassiveClusterNode(ownId, setting, recovered,
        eventIdGenerator, initialFileEventId,
        otherFailedOver, journalConf, clusterConf, config, common)
      passiveOrWorkingNode := Some(Left(node))
      node
    }

    for (prepared <- prepare()) yield
      Service.resource(Task(
        new ClusterNode(prepared, passiveOrWorkingNode,
          journalConf, clusterConf,
          eventIdGenerator, eventBus.narrowPublisher, common)))
  }

  // TODO Provisional fix because it's not easy to restart the recovery
  final class RestartAfterJournalTruncationException
  extends RuntimeException("Restart after journal truncation")
  with NoStackTrace

  final case class ClusterWatchConfirmed(
    command: ClusterWatchConfirm,
    result: Checked[Unit])

  private case object ClusterNodeCanceledProblem extends Problem.ArgumentlessCoded

  private final case class Prepared[S <: SnapshotableState[S]](
    currentPassiveReplicatedState: Task[Option[Checked[S]]],
    untilActiveNodeRecovered: Task[Checked[Recovered[S]]],
    expectingStartBackupCommand: Option[Promise[ClusterStartBackupNode]] = None)
}
