package js7.cluster

import akka.actor.ActorSystem
import akka.util.Timeout
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{ExitCase, Resource}
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import com.softwaremill.diffx
import izumi.reflect.Tag
import java.nio.file.Path
import js7.base.auth.{Admission, UserId}
import js7.base.eventbus.EventPublisher
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.service.Service.Started
import js7.base.service.{MainServiceTerminationException, Service}
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination}
import js7.cluster.ClusterConf.ClusterProductName
import js7.cluster.ClusterNode.*
import js7.cluster.JournalTruncator.truncateJournal
import js7.core.license.LicenseChecker
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotActiveProblem, ClusterNodeIsNotBackupProblem, ClusterNodeIsNotReadyProblem, PassiveClusterNodeResetProblem, PrimaryClusterNodeMayNotBecomeBackupProblem}
import js7.data.cluster.ClusterCommand.{ClusterInhibitActivation, ClusterStartBackupNode}
import js7.data.cluster.ClusterState.{Coupled, Empty, FailedOver, HasNodes}
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterCommand, ClusterNodeApi, ClusterSetting, ClusterWatchRequest, ClusterWatchingCommand}
import js7.data.event.{AnyKeyedEvent, ClusterableState, EventId, JournalPosition, Stamped}
import js7.data.node.{NodeName, NodeNameToPassword}
import js7.journal.data.JournalLocation
import js7.journal.recover.{Recovered, StateRecoverer}
import js7.journal.{EventIdClock, EventIdGenerator}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import scala.concurrent.Promise
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

final class ClusterNode[S <: ClusterableState[S]: diffx.Diff: Tag] private(
  prepared: Prepared[S],
  passiveOrWorkingNode: AtomicAny[Option[Either[PassiveClusterNode[S], Allocated[Task, WorkingClusterNode[S]]]]],
  currentStateRef: Ref[Task, Task[Either[Problem, S]]],
  val clusterConf: ClusterConf,
  eventIdGenerator: EventIdGenerator,
  eventBus: EventPublisher[Stamped[AnyKeyedEvent]],
  common: ClusterCommon,
  val recoveredExtract: Recovered.Extract,
  implicit val actorSystem: ActorSystem)
  (implicit S: ClusterableState.Companion[S],
    nodeNameToPassword: NodeNameToPassword[S],
    scheduler: Scheduler,
    timeout: akka.util.Timeout)
extends Service.StoppableByRequest
{
  clusterNode =>

  import clusterConf.ownId
  import common.activationInhibitor

  private val workingNodeStarted =
    Deferred.unsafe[Task, Try[Either[ProgramTermination, WorkingClusterNode[S]]]]
  private var _testDontNotifyActiveNodeAboutShutdown = false // for test only
  private val recoveryStopRequested = Deferred.unsafe[Task, Unit]

  def dontNotifyActiveNodeAboutShutdown(): Unit =
    _testDontNotifyActiveNodeAboutShutdown = true

  val currentState: Task[Either[Problem, S]] =
    currentStateRef.get.flatten

  /** None when stopped before activated. */
  def untilActivated: Task[Either[ProgramTermination, WorkingClusterNode[S]]] =
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
  protected def start: Task[Started] =
    startService(
      untilWorkingNodeStarted
        .onErrorRecover {
          case ProblemException(prblm @ PassiveClusterNodeResetProblem) =>
            // untilWorkingNodeStarted has logged and handled PassiveClusterNodeResetProblem
            logger.debug(prblm.toString)
        }
        .start
        .flatMap(fiber =>
          Task.race(recoveryStopRequested.get, fiber.join))
        .*>(untilStopRequested)
        .guaranteeCase(exitCase =>
          stopRecovery(ProgramTermination()/*???*/) *>
            Task.defer(passiveOrWorkingNode.get() match {
              case Some(Left(passiveClusterNode)) if exitCase == ExitCase.Completed =>
                passiveClusterNode.onShutdown(_testDontNotifyActiveNodeAboutShutdown)

              case Some(Right(workingClusterNodeAllocated)) =>
                workingClusterNodeAllocated.release

              case _ => Task.unit
            })))

  private def untilWorkingNodeStarted: Task[Unit] =
    logger.debugTask(untilRecovered
      .flatMap(startWorkingNode)
      .materialize
      .flatTap(triedWorkingNode =>
        workingNodeStarted
          .complete(triedWorkingNode match {
            case Failure(prblm @ ProblemException(PassiveClusterNodeResetProblem)) =>
              logger.warn(s"Should restart after $prblm")
              Success(Left(ProgramTermination(restart = true)))
            case Failure(t) => Failure(t)
            case Success(o) => Success(Right(o))
          })
          .attempt)
      .dematerialize
      .flatMap(workingNode =>
        currentStateRef.set(workingNode.journal.state.map(Right(_)))))

  private def untilRecovered: Task[Recovered[S]] =
    logger.debugTask(prepared
      .untilRecovered
      .map(_.orThrowWithoutOurStackTrace)
      .flatTap(recovered => Task.raiseUnless(recovered.clusterState.isEmptyOrActive(ownId))(
        new IllegalStateException("Controller has recovered from Journal but is not the " +
          s"active node in ClusterState: id=$ownId, failedOver=${recovered.clusterState}"))))

  def stopRecovery(termination: ProgramTermination): Task[Unit] =
    Task.defer {
      logger.trace("stopRecovery")
      recoveryStopRequested.complete(()).attempt *>
        workingNodeStarted.complete(Success(Left(termination))).attempt.void
    }

  private def startWorkingNode(recovered: Recovered[S]): Task[WorkingClusterNode[S]] =
    logger.traceTask(
      WorkingClusterNode
        .resource(
          recovered, common, clusterConf, eventIdGenerator, eventBus)
        .toAllocated
        .flatTap(allocated => Task {
          passiveOrWorkingNode := Some(Right(allocated))
        })
        .map(_.allocatedThing))

  def workingClusterNode: Checked[WorkingClusterNode[S]] =
    passiveOrWorkingNode.get()
      .flatMap(_.toOption)
      .map(_.allocatedThing)
      .toRight(ClusterNodeIsNotActiveProblem)

  def executeCommand(command: ClusterCommand): Task[Checked[ClusterCommand.Response]] = {
    logger.infoTask(command.getClass.simpleScalaName)(
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
                  .flatMap(_.traverse(_.journal.clusterState))
                  .map(Some(_))
                  .timeoutTo(duration /*???*/ - 500.ms, Task.none)
                  .flatMap {
                    case None =>
                      Task.left(Problem.pure(
                        "ClusterInhibitActivation timed out — please try again"))

                    case Some(Left(_: ProgramTermination)) =>
                      // No journal
                      Task.left(Problem.pure(
                        "ClusterInhibitActivation command failed due to cluster is being terminated"))

                    case Some(Right(failedOver: FailedOver)) =>
                      logger.debug(s"inhibitActivation(${duration.pretty}) => $failedOver")
                      Task.right(ClusterInhibitActivation.Response(Some(failedOver)))

                    case Some(Right(clusterState)) =>
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
      })
  }

  def executeClusterWatchingCommand(command: ClusterWatchingCommand): Task[Checked[Unit]] =
    command match {
      case cmd: ClusterWatchConfirm =>
        Task(passiveOrWorkingNode.get())
          .flatMap {
            case Some(Right(workingClusterNodeAllocated)) =>
              workingClusterNodeAllocated.allocatedThing.executeClusterWatchConfirm(cmd)
            case _ =>
              common.clusterWatchCounterpart.executeClusterWatchConfirm(cmd)
          }
          .tapEval(result => Task(
            common.testEventBus.publish(ClusterWatchConfirmed(cmd, result))))
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

  def recoveringResource[S <: ClusterableState[S] : diffx.Diff : Tag](
    akkaResource: Resource[Task, ActorSystem],
    clusterNodeApi: (Admission, String, ActorSystem) => Resource[Task, ClusterNodeApi],
    licenseChecker: LicenseChecker,
    journalLocation: JournalLocation,
    clusterConf: ClusterConf,
    eventIdClock: EventIdClock,
    testEventBus: EventPublisher[Any])
    (implicit
      S: ClusterableState.Companion[S],
      nodeNameToPassword: NodeNameToPassword[S],
      scheduler: Scheduler,
      akkaTimeout: Timeout)
  : Resource[Task, ClusterNode[S]] =
    StateRecoverer
      .resource[S](journalLocation, clusterConf.config)
      .parZip(akkaResource/*start in parallel*/)
      .flatMap { case (recovered, actorSystem) =>
        implicit val a = actorSystem
        resource(
          recovered,
          clusterNodeApi(_, _, actorSystem),
          licenseChecker, journalLocation, clusterConf, eventIdClock, testEventBus
        ).orThrow
      }

  private def resource[S <: ClusterableState[S] : diffx.Diff : Tag](
    recovered: Recovered[S],
    clusterNodeApi: (Admission, String) => Resource[Task, ClusterNodeApi],
    licenseChecker: LicenseChecker,
    journalLocation: JournalLocation,
    clusterConf: ClusterConf,
    eventIdClock: EventIdClock,
    testEventBus: EventPublisher[Any])
    (implicit
      S: ClusterableState.Companion[S],
      nodeNameToPassword: NodeNameToPassword[S],
      scheduler: Scheduler,
      actorSystem: ActorSystem,
      akkaTimeout: akka.util.Timeout)
  : Checked[Resource[Task, ClusterNode[S]]] = {
    val checked = recovered.clusterState match {
      case Empty =>
        (clusterConf.isPrimary || recovered.eventId == EventId.BeforeFirst) !!
          PrimaryClusterNodeMayNotBecomeBackupProblem

      case clusterState: HasNodes =>
        import clusterConf.ownId
        licenseChecker.checkLicense(ClusterProductName) >>
          ((ownId == clusterState.activeId || ownId == clusterState.passiveId) !! Problem.pure(
            s"Own cluster $ownId does not match clusterState=${recovered.clusterState}"))
    }

    for (_ <- checked) yield
      for {
        clusterWatchCounterpart <-
          ClusterWatchCounterpart.resource(clusterConf, clusterConf.timing, testEventBus)
        common <- ClusterCommon.
          resource(clusterWatchCounterpart, clusterNodeApi, clusterConf,
          licenseChecker, testEventBus)
        clusterNode <- resource(recovered, common, journalLocation, clusterConf,
          new EventIdGenerator(eventIdClock),
          testEventBus)
      } yield clusterNode
  }

  private def resource[S <: ClusterableState[S] : diffx.Diff : Tag](
    recovered: Recovered[S],
    common: ClusterCommon,
    journalLocation: JournalLocation,
    clusterConf: ClusterConf,
    eventIdGenerator: EventIdGenerator,
    eventBus: EventPublisher[Any])
    (implicit
      S: ClusterableState.Companion[S],
      nodeNameToPassword: NodeNameToPassword[S],
      scheduler: Scheduler,
      actorSystem: ActorSystem,
      timeout: akka.util.Timeout)
  : Resource[Task, ClusterNode[S]] = {
    import clusterConf.{config, ownId}

    if (recovered.clusterState != Empty) logger.info(
      s"This is cluster $ownId, recovered ClusterState is ${recovered.clusterState}")

    val keepTruncatedRest = config.getBoolean("js7.journal.cluster.keep-truncated-rest")
    val passiveOrWorkingNode = AtomicAny[Option[
      Either[PassiveClusterNode[S], Allocated[Task, WorkingClusterNode[S]]]]](None)

    import common.activationInhibitor

    def prepareBackupNodeWithEmptyClusterState(): Prepared[S] = {
      logger.info(s"Backup cluster $ownId, awaiting appointment from a primary node")
      val startedPromise = Promise[ClusterStartBackupNode]()
      val passiveClusterNode = Task.fromFuture(startedPromise.future)
        .map(cmd =>
          newPassiveClusterNode(recovered, cmd.setting,
            initialFileEventId = Some(cmd.fileEventId),
            injectedPassiveUserId = Some(cmd.passiveNodeUserId),
            injectedActiveNodeName = Some(cmd.activeNodeName)))
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
        untilRecovered = untilActiveRecovered,
        expectingStartBackupCommand = Some(startedPromise))
    }

    def startAsActiveNodeWithBackup(): Prepared[S] =
      recovered.clusterState match {
        case recoveredClusterState: Coupled =>
          import recoveredClusterState.passiveId
          logger.info(s"This cluster $ownId was active and coupled before restart - " +
            s"asking $passiveId about its state")

          val failedOver = common
            .inhibitActivationOfPeer(
              recoveredClusterState,
              recovered.state
                .clusterNodeToUserAndPassword(
                  ourNodeId = recoveredClusterState.activeId,
                  otherNodeId = recoveredClusterState.passiveId)
                .orThrow)
            .map {
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
            untilRecovered =
              failedOver.flatMap {
                case None => Task.right(recovered)
                case Some((ourRecovered, passiveClusterNode)) =>
                  passiveClusterNode.run(ourRecovered.state)
              })

        case _ =>
          logger.info("Remaining the active cluster node, not coupled with passive node")
          Prepared(
            currentPassiveReplicatedState = Task.none,
            untilRecovered = activationInhibitor.startActive.as(Right(recovered)))
      }

    def startPassiveAfterFailover(coupled: Coupled, otherFailedOver: FailedOver)
    : (Recovered[S], PassiveClusterNode[S]) = {
      logger.warn(s"The other ${otherFailedOver.activeId} ${otherFailedOver.toShortString}" +
        ", and became active while this node was absent")
      assertThat(otherFailedOver.idToUri == coupled.idToUri &&
        otherFailedOver.activeId == coupled.passiveId)
      // This restarted, previously failed active cluster node may have written one chunk of events
      // more than the passive node, maybe even an extra snapshot in a new journal file.
      // These extra events are not acknowledged. So we truncate our journal.
      val ourRecovered = truncateJournalAndRecoverAgain(otherFailedOver) match {
        case None => recovered
        case Some(truncatedRecovered) =>
          assertThat(truncatedRecovered.state.clusterState == coupled)
          assertThat(!recovered.eventWatch.whenStarted.isCompleted)
          recovered.close() // Should do nothing, because recovered.eventWatch has not been started
          truncatedRecovered
      }
      ourRecovered -> newPassiveClusterNode(ourRecovered, otherFailedOver.setting,
        otherFailedOver = true)
    }

    def truncateJournalAndRecoverAgain(otherFailedOver: FailedOver): Option[Recovered[S]] =
      for (file <- truncateJournal(journalLocation.fileBase, otherFailedOver.failedAt, keepTruncatedRest))
        yield recoverFromTruncated(file, otherFailedOver.failedAt)

    def recoverFromTruncated(file: Path, failedAt: JournalPosition): Recovered[S] = {
      logger.info("Recovering again after unacknowledged events have been deleted properly from journal file")
      throw new RestartAfterJournalTruncationException

      // May take a long time !!!
      val recovered = StateRecoverer.recover[S](journalLocation, config)

      // Assertions
      val recoveredJournalFile = recovered.recoveredJournalFile
        .getOrElse(sys.error(s"Unrecoverable journal file: ${file.getFileName}"))
      assertThat(recoveredJournalFile.file == file)
      assertThat(recoveredJournalFile.journalPosition == failedAt,
        s"${recoveredJournalFile.journalPosition} != $failedAt")

      recovered
    }

    def preparePassiveNode(recovered: Recovered[S], clusterState: HasNodes): Prepared[S] = {
      logger.info(
        if (clusterState.isInstanceOf[Coupled])
          s"Remaining a passive cluster node following the active ${clusterState.activeId}"
        else
          s"Remaining a passive cluster node trying to follow the active ${clusterState.activeId}")
      val passive = newPassiveClusterNode(recovered, clusterState.setting)
      Prepared(
        currentPassiveReplicatedState = passive.state.map(s => Some(Right(s))),
        untilRecovered = passive.run(recovered.state))
    }

    def newPassiveClusterNode(
      recovered: Recovered[S],
      setting: ClusterSetting,
      otherFailedOver: Boolean = false,
      initialFileEventId: Option[EventId] = None,
      injectedActiveNodeName: Option[NodeName] = None,
      injectedPassiveUserId: Option[UserId] = None)
    : PassiveClusterNode[S] = {
      assertThat(!passiveOrWorkingNode.get().exists(_.isLeft))
      val node = new PassiveClusterNode(ownId, setting, recovered,
        activeNodeName = injectedActiveNodeName getOrElse
          recovered.state.clusterNodeIdToName(setting.activeId).orThrow,
        passiveUserId = injectedPassiveUserId getOrElse
          recovered.state.clusterNodeToUserId(setting.passiveId).orThrow,
        eventIdGenerator, initialFileEventId,
        otherFailedOver, clusterConf, common)
      passiveOrWorkingNode := Some(Left(node))
      node
    }

    val prepared = recovered.clusterState match {
      case Empty =>
        if (clusterConf.isPrimary) {
          logger.debug(s"Active primary cluster $ownId, no backup node appointed")
          Prepared(
            currentPassiveReplicatedState = Task.none,
            untilRecovered = activationInhibitor.startActive.as(Right(recovered)))
        } else
          prepareBackupNodeWithEmptyClusterState()

      case clusterState: HasNodes =>
        if (ownId == clusterState.activeId)
          startAsActiveNodeWithBackup()
        else
          preparePassiveNode(recovered, clusterState)
    }

    Service.resource(
      for {
        currentStateRef <- Ref[Task].of(
          prepared.currentPassiveReplicatedState
            .map(_.toChecked(ClusterNodeIsNotReadyProblem).flatten))
      } yield new ClusterNode(
        prepared, passiveOrWorkingNode, currentStateRef,
        clusterConf,
        eventIdGenerator, eventBus.narrowPublisher, common, recovered.extract, actorSystem))
  }

  // TODO Provisional fix because it's not easy to restart the recovery
  // ServiceMain catches this exception by its `MainServiceTerminationException` trait !!!
  final class RestartAfterJournalTruncationException
  extends RuntimeException("Restart after journal truncation")
  with MainServiceTerminationException
  with NoStackTrace {
    def termination = ProgramTermination(restart = true)
  }

  final case class ClusterWatchConfirmed(
    command: ClusterWatchConfirm,
    result: Checked[Unit])

  private final case class Prepared[S <: ClusterableState[S]](
    currentPassiveReplicatedState: Task[Option[Checked[S]]],
    untilRecovered: Task[Checked[Recovered[S]]],
    expectingStartBackupCommand: Option[Promise[ClusterStartBackupNode]] = None)
}
