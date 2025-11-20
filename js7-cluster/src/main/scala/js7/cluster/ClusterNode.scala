package js7.cluster

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, Outcome, Ref, Resource, ResourceIO, SyncIO}
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import izumi.reflect.Tag
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
import js7.base.auth.{Admission, UserId}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.CatsExtensions.{tryIt, untry}
import js7.base.catsutils.UnsafeMemoizable
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.eventbus.EventPublisher
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.log.log4j.Log4j
import js7.base.monixlike.MonixLikeExtensions.*
import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.service.{MainServiceTerminationException, Service}
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, Atomic, ProgramTermination}
import js7.cluster.ClusterConf.ClusterProductName
import js7.cluster.ClusterNode.*
import js7.cluster.JournalTruncator.truncateJournal
import js7.core.license.LicenseChecker
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotActiveProblem, ClusterNodeIsNotBackupProblem, ClusterNodeIsNotReadyProblem, PassiveClusterNodeResetProblem, PrimaryClusterNodeMayNotBecomeBackupProblem}
import js7.data.cluster.ClusterCommand.{ClusterInhibitActivation, ClusterStartBackupNode}
import js7.data.cluster.ClusterState.{Coupled, Empty, FailedOver, HasNodes}
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterCommand, ClusterNodeApi, ClusterSetting, ClusterState, ClusterWatchRequest, ClusterWatchingCommand}
import js7.data.event.{AnyKeyedEvent, ClusterableState, EventId, JournalPosition, Stamped}
import js7.data.node.{NodeName, NodeNameToPassword}
import js7.journal.EventIdGenerator
import js7.journal.data.JournalLocation
import js7.journal.recover.{Recovered, StateRecoverer}
import org.apache.pekko
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.Promise
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

final class ClusterNode[S <: ClusterableState[S]: Tag] private(
  prepared: Prepared[S],
  passiveOrWorkingNode: AtomicReference[Option[Either[
    Allocated[SyncIO, PassiveClusterNode[S]],
    Allocated[IO, WorkingClusterNode[S]]]]],
  currentStateRef: Ref[IO, IO[Checked[S]]],
  val clusterConf: ClusterConf,
  eventIdGenerator: EventIdGenerator,
  eventBus: EventPublisher[Stamped[AnyKeyedEvent]],
  common: ClusterCommon,
  shuttingDown: Deferred[IO, Unit],
  val recoveredExtract: Recovered.Extract,
  implicit val actorSystem: ActorSystem)
  (implicit S: ClusterableState.Companion[S],
    nodeNameToPassword: NodeNameToPassword[S])
extends Service.StoppableByRequest:
  clusterNode =>

  import clusterConf.ownId
  import common.activationConsentChecker.activationInhibitor

  private val workingNodeStarted =
    Deferred.unsafe[IO, Try[Either[ProgramTermination, WorkingClusterNode[S]]]]
  private var _dontNotifyActiveNodeAboutShutdown = false // for test only

  val currentState: IO[Checked[S]] =
    currentStateRef.get.flatten

  /** Returns Left when stopped before activated. */
  def untilActivated: IO[Either[ProgramTermination, WorkingClusterNode[S]]] =
    logger.traceIOWithResult("untilActivated"):
      workingNodeStarted.get.dematerialize.recover:
        case t: RestartAfterJournalTruncationException =>
          logger.info(t.getMessage)
          Left(ProgramTermination.Restart)

  protected def start =
    startService(run)

  private def run =
    untilWorkingNodeStarted
      .recover:
        case ProblemException(prblm @ PassiveClusterNodeResetProblem) =>
          // untilWorkingNodeStarted has logged and handled PassiveClusterNodeResetProblem
          logger.debug(prblm.toString)
      // TODO How to cancel while still waiting for appointment?
      //  Test with PassiveLostControllerClusterTest.
      //.background.surround:
      //  untilStopRequested
      // Following lines do not cancel untilWorkingNodeStarted:
      .start
      .flatMap: fiber =>
        IO.race(untilStopRequested, fiber.joinStd)
      .*>(untilStopRequested)
      .guaranteeCaseLazy: outcome =>
        logger.debugIO("run guarantee", outcome):
          announceShutdown(ProgramTermination()/*???*/) *>
            IO.defer:
              (passiveOrWorkingNode.get(), outcome) match
                case (Some(Left(passiveClusterNode)), Outcome.Succeeded(_)) =>
                  passiveClusterNode.allocatedThing.onShutdown(_dontNotifyActiveNodeAboutShutdown)
                    *> passiveClusterNode.release.to[IO]

                case (Some(Right(workingClusterNodeAllocated)), _) =>
                  workingClusterNodeAllocated.release

                case _ => IO.unit
            //? *> stopWorkingClusterNode

  private def stopWorkingClusterNode: IO[Unit] =
    logger.traceIO:
      workingNodeStarted.tryGet.flatMap:
        case Some(Success(Right(workingClusterNode))) =>
          workingClusterNode.stop
            .logWhenItTakesLonger("workingClusterNode.stop")
        case _ => IO.unit

  private def untilWorkingNodeStarted: IO[Unit] =
    logger.debugIO:
      untilRecovered
        .flatMap:
          startWorkingNode
        .tryIt
        .flatTap: triedWorkingNode =>
          workingNodeStarted.complete:
            triedWorkingNode match
              case Failure(prblm @ ProblemException(PassiveClusterNodeResetProblem)) =>
                logger.warn(s"Should restart after $prblm")
                Success(Left(ProgramTermination(restart = true)))
              case Failure(t) => Failure(t)
              case Success(o) => Success(Right(o))
        .untry
        .flatMap: workingNode =>
          currentStateRef.set(workingNode.journal.aggregate.map(Right(_)))

  private def untilRecovered: IO[Recovered[S]] =
    logger.debugIO:
      prepared.untilRecovered
        .map(_.orThrowWithoutOurStackTrace)
        .flatTap: recovered =>
          IO.raiseUnless(recovered.clusterState.isEmptyOrActive(ownId)):
            new IllegalStateException("Controller has recovered from Journal but is not the " +
              s"active node in ClusterState: id=$ownId, failedOver=${recovered.clusterState}")

  /** In case we stick in ClusterWatchCounterPart. */
  def announceShutdown(termination: ProgramTermination, dontNotifyActiveNode: Boolean = false)
  : IO[Unit] =
    IO.defer:
      logger.traceIO("announceShutdown")
      if dontNotifyActiveNode then
        _dontNotifyActiveNodeAboutShutdown = true
      shuttingDown.complete(()) *>
        // if not yet started:
        workingNodeStarted.complete(Success(Left(termination))).void

  private def startWorkingNode(recovered: Recovered[S]): IO[WorkingClusterNode[S]] =
    logger.traceIO:
      passiveOrWorkingNode.get.flatMap(_.left.toOption).foldMap(_.release.to[IO]) *>
        WorkingClusterNode.resource(recovered, common, clusterConf, eventIdGenerator)
          .toAllocated.flatTap: allocated =>
            IO:
              passiveOrWorkingNode := Some(Right(allocated))
          .map(_.allocatedThing)

  def workingClusterNode: Checked[WorkingClusterNode[S]] =
    passiveOrWorkingNode.get()
      .flatMap(_.toOption)
      .map(_.allocatedThing)
      .toRight(ClusterNodeIsNotActiveProblem)

  def executeCommand(command: ClusterCommand): IO[Checked[ClusterCommand.Response]] =
    logger.infoIO(s"executeCommand ${command.toShortString}"):
      command match
        case command: ClusterCommand.ClusterStartBackupNode =>
          IO:
            prepared.expectingStartBackupCommand match
              case None =>
                if !clusterConf.isBackup then
                  Left(ClusterNodeIsNotBackupProblem)
                else
                  Left(Problem.pure("Cluster node is not ready to accept a backup node configuration"))
              case Some(promise) =>
                if command.setting.passiveId != ownId then
                  Left(Problem.pure(s"$command sent to wrong $ownId"))
                else if command.setting.activeId == ownId then
                  Left(Problem.pure(s"$command must not be sent to the active node"))
                else
                  promise.trySuccess(command)
                  Right(ClusterCommand.Response.Accepted)

        case ClusterCommand.ClusterConfirmCoupling(token) =>
          IO:
            passiveOrWorkingNode.get() match
              case Some(Left(passive)) => passive.allocatedThing.confirmCoupling(token)
              case _ => Left(Problem("Not a passive cluster node"))
          .rightAs(ClusterCommand.Response.Accepted)

        case ClusterCommand.ClusterInhibitActivation(duration) =>
          activationInhibitor.inhibitActivation(duration).flatMap: inhibited =>
            if inhibited then
              IO.pure(Right(ClusterInhibitActivation.Response(clusterState = None)))
            else
              workingNodeStarted.get
                .dematerialize
                .flatMap(_.traverse(_.journal.clusterState))
                .map(Some(_))
                // Because the ActivationInhibter is Active state, workingNodeStarted is expected
                // to be fill nearly immediately.
                .timeoutTo(duration /*???*/, IO.none)
                .flatMap:
                  case None =>
                    IO.left(Problem.pure:
                      "ClusterInhibitActivation command timed out — please try again")

                  case Some(Left(_: ProgramTermination)) =>
                    // No journal
                    IO.left(Problem.pure:
                      "ClusterInhibitActivation command failed due to program termination")

                  case Some(Right(clusterState)) =>
                    logger.debug(s"inhibitActivation(${duration.pretty}) => $clusterState")
                    IO.right(ClusterInhibitActivation.Response(Some(clusterState)))

        case _: ClusterCommand.ClusterPrepareCoupling |
             _: ClusterCommand.ClusterCouple |
             _: ClusterCommand.ClusterRecouple |
             _: ClusterCommand.ClusterPassiveDown =>

          if _dontNotifyActiveNodeAboutShutdown then
          // Avoid immediate recoupling
            IO.left(Problem(
              s"${command.getClass.simpleScalaName} command rejected due to dontNotifyActiveNode"))
          else
            IO.pure(workingClusterNode)
              .flatMapT(_.executeCommand(command))

  def executeClusterWatchingCommand(command: ClusterWatchingCommand): IO[Checked[Unit]] =
    command match
      case cmd: ClusterWatchConfirm =>
        IO(passiveOrWorkingNode.get())
          .flatMap:
            case Some(Right(workingClusterNodeAllocated)) =>
              workingClusterNodeAllocated.allocatedThing.executeClusterWatchConfirm(cmd)
            case _ =>
              common.clusterWatchCounterpart.executeClusterWatchConfirm(cmd)
          .flatTap(result => IO:
            common.testEventBus.publish(ClusterWatchConfirmed(cmd, result)))

  def clusterWatchRequestStream: fs2.Stream[IO, ClusterWatchRequest] =
    common.clusterWatchCounterpart.newStream

  /** Is the active or non-cluster (Empty, isPrimary) node or is becoming active. */
  def isWorkingNode: Boolean = passiveOrWorkingNode.get().exists(_.isRight)

  def isPassive: Boolean = passiveOrWorkingNode.get().exists(_.isLeft)

  override def toString = s"ClusterNode(${ownId.string})"


object ClusterNode:
  private val logger = Logger[this.type]

  def recoveringResource[S <: ClusterableState[S]: Tag](
    pekkoResource: ResourceIO[ActorSystem],
    clusterNodeApi: (Admission, String, ActorSystem) => ResourceIO[ClusterNodeApi],
    licenseChecker: LicenseChecker,
    journalLocation: JournalLocation,
    clusterConf: ClusterConf,
    eventIdGenerator: EventIdGenerator,
    testEventBus: EventPublisher[Any])
    (implicit
      S: ClusterableState.Companion[S],
      nodeNameToPassword: NodeNameToPassword[S],
      ioRuntime: IORuntime)
  : ResourceIO[ClusterNode[S]] =
    for
      _ <- Resource.eval(IO:
        Log4j.set("js7.clusterNodeId", clusterConf.ownId.string))
      clusterNode <- StateRecoverer
        .resource[S](journalLocation, clusterConf.config)
        .both(pekkoResource/*start in parallel*/)
        .flatMap:
          case (recovered, actorSystem) =>
            given ActorSystem = actorSystem
            resource(
              recovered,
              clusterNodeApi(_, _, actorSystem),
              licenseChecker, journalLocation, clusterConf, eventIdGenerator, testEventBus
            ).orThrow
    yield
      clusterNode

  private def resource[S <: ClusterableState[S]: Tag](
    recovered: Recovered[S],
    clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
    licenseChecker: LicenseChecker,
    journalLocation: JournalLocation,
    clusterConf: ClusterConf,
    eventIdGenerator: EventIdGenerator,
    testEventBus: EventPublisher[Any])
    (using
      S: ClusterableState.Companion[S],
      nodeNameToPassword: NodeNameToPassword[S],
      ioRuntime: IORuntime,
      actorSystem: ActorSystem)
  : Checked[ResourceIO[ClusterNode[S]]] =
    val checked = recovered.clusterState match
      case Empty =>
        (clusterConf.isPrimary || recovered.eventId == EventId.BeforeFirst) !!
          PrimaryClusterNodeMayNotBecomeBackupProblem

      case clusterState: HasNodes =>
        import clusterConf.ownId
        licenseChecker.checkLicense(ClusterProductName) >>
          ((ownId == clusterState.activeId || ownId == clusterState.passiveId) !! Problem.pure(
            s"Own cluster $ownId does not match clusterState=${recovered.clusterState}"))

    checked.map: _ =>
      for
        shuttingDown <- Resource.eval(Deferred[IO, Unit])
        clusterWatchCounterpart <-
          ClusterWatchCounterpart.resource(clusterConf, clusterConf.timing, shuttingDown,
            testEventBus)
        common <- ClusterCommon.
          resource(clusterWatchCounterpart, clusterNodeApi, clusterConf,
            licenseChecker, testEventBus)
        clusterNode <- resource(recovered, common, journalLocation, clusterConf,
          eventIdGenerator, shuttingDown,
          testEventBus)
      yield
        clusterNode

  private def resource[S <: ClusterableState[S]: Tag](
    recovered: Recovered[S],
    common: ClusterCommon,
    journalLocation: JournalLocation,
    clusterConf: ClusterConf,
    eventIdGenerator: EventIdGenerator,
    shuttingDown: Deferred[IO, Unit],
    eventBus: EventPublisher[Any])
    (using
      S: ClusterableState.Companion[S],
      nodeNameToPassword: NodeNameToPassword[S],
      ioRuntime: IORuntime,
      actorSystem: ActorSystem)
  : ResourceIO[ClusterNode[S]] =
    import clusterConf.{config, ownId}

    if recovered.clusterState != Empty then logger.info(
      s"This is cluster $ownId, recovered ClusterState is ${recovered.clusterState}")

    val keepTruncatedRest = config.getBoolean("js7.journal.cluster.keep-truncated-rest")
    val passiveOrWorkingNode = Atomic[Option[
      Either[Allocated[SyncIO, PassiveClusterNode[S]], Allocated[IO, WorkingClusterNode[S]]]]](None)

    import common.activationConsentChecker.activationInhibitor

    def prepareBackupNodeWithEmptyClusterState(): Prepared[S] =
      logger.info(s"Backup cluster $ownId, awaiting appointment from a primary node")
      val startedPromise = Promise[ClusterStartBackupNode]()
      val passiveClusterNode: IO[PassiveClusterNode[S]] =
        memoize:
          IO.fromFuture(IO.pure(startedPromise.future))
            .map: cmd =>
              newPassiveClusterNode(recovered, cmd.setting,
                initialFileEventId = Some(cmd.fileEventId),
                injectedPassiveUserId = Some(cmd.passiveNodeUserId),
                injectedActiveNodeName = Some(cmd.activeNodeName))
      val currentPassiveState = IO.defer:
        if startedPromise.future.isCompleted then
          passiveClusterNode.flatMap(_.state.map(s => Some(Right(s))))
        else
          IO.some(Left(BackupClusterNodeNotAppointed))
      val untilActiveRecovered =
        passiveClusterNode.flatMap: passive =>
          activationInhibitor.startPassive *>
            passive.run(recovered.state)

      Prepared(
        currentPassiveReplicatedState = currentPassiveState,
        untilRecovered = untilActiveRecovered,
        expectingStartBackupCommand = Some(startedPromise))

    def startAsActiveNodeWithBackup: Prepared[S] =
      recovered.clusterState match
        case clusterState: Coupled =>
          import clusterState.passiveId
          logger.info(s"This cluster $ownId was active and coupled before restart - " +
            s"asking $passiveId about its state")

          val startedAsPassive: IO[Option[(Recovered[S], PassiveClusterNode[S])]] =
            memoize:
              common.inhibitActivationOfPeer(
                  clusterState,
                  recovered.state
                    .clusterNodeToUserAndPassword(
                      ourNodeId = clusterState.activeId,
                      otherNodeId = clusterState.passiveId)
                    .orThrow)
                .map:
                  case None /*Other node has not failed-over*/ =>
                    logger.info(s"✔︎ The other $passiveId is up and still passive, " +
                      "so this node remains the active cluster node")
                    None

                  case Some(peerClusterState: HasNodes) =>
                    Some(startPassiveBecausePeerIsActive(clusterState, peerClusterState))

                  case Some(ClusterState.Empty) =>
                    // TODO Maybe this, together with NodesAppointed(?) means the peer's journal
                    //  has been reset?
                    throw new RuntimeException(
                      s"The peer node's ClusterState.Empty does not match our ${
                        clusterState.toShortString}")

          Prepared(
            currentPassiveReplicatedState =
              startedAsPassive.flatMap:
                case None => IO.none
                case Some((_, passiveClusterNode)) =>
                  passiveClusterNode.state.map(s => Some(Right(s))),
            untilRecovered =
              startedAsPassive.flatMap:
                case None => IO.right(recovered)
                case Some((ourRecovered, passiveClusterNode)) =>
                  passiveClusterNode.run(ourRecovered.state))

        case _ =>
          logger.info("✔︎ Remaining the active cluster node, not coupled with passive node")
          Prepared(
            currentPassiveReplicatedState = IO.none,
            untilRecovered = activationInhibitor.startActive.as(Right(recovered)))

    def startPassiveBecausePeerIsActive(ourClusterState: Coupled, peerClusterState: HasNodes)
    : (Recovered[S], PassiveClusterNode[S]) =
      logger.warn(s"The other ${peerClusterState.activeId} ${peerClusterState.toShortString}" +
        ", and became active while this node was absent")
      assertThat(peerClusterState.idToUri == ourClusterState.idToUri &&
        peerClusterState.activeId == ourClusterState.passiveId)
      // This restarted, previously failed active cluster node may have written one chunk of events
      // more than the passive node, maybe even an extra snapshot in a new journal file.
      // These extra events are not acknowledged. So we truncate our journal.
      val ourRecovered = peerClusterState match
        case peerClusterState: FailedOver =>
          truncateJournalAndRecoverAgain(peerClusterState) match
            case None => recovered
            case Some(truncatedRecovered) =>
              assertThat(truncatedRecovered.state.clusterState == ourClusterState)
              assertThat(!recovered.eventWatch.whenStarted.isCompleted)
              recovered.close() // Should do nothing, because recovered.eventWatch has not been started
              truncatedRecovered
        case _: ClusterState =>
          recovered
      ourRecovered -> newPassiveClusterNode(ourRecovered, peerClusterState.setting,
        otherFailedOver = true)

    def truncateJournalAndRecoverAgain(otherFailedOver: FailedOver): Option[Recovered[S]] =
      for file <- truncateJournal(journalLocation.fileBase, otherFailedOver.failedAt, keepTruncatedRest)
        yield recoverFromTruncated(file, otherFailedOver.failedAt)

    def recoverFromTruncated(file: Path, failedAt: JournalPosition): Recovered[S] =
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

    def preparePassiveNode(recovered: Recovered[S], clusterState: HasNodes): Prepared[S] =
      logger.info(
        if clusterState.isInstanceOf[Coupled] then
          s"Remaining a passive cluster node following the active ${clusterState.activeId}"
        else
          s"Remaining a passive cluster node trying to follow the active ${clusterState.activeId}")
      val passive = newPassiveClusterNode(recovered, clusterState.setting)
      Prepared(
        currentPassiveReplicatedState = passive.state.map(s => Some(Right(s))),
        untilRecovered = passive.run(recovered.state))

    def newPassiveClusterNode(
      recovered: Recovered[S],
      setting: ClusterSetting,
      otherFailedOver: Boolean = false,
      initialFileEventId: Option[EventId] = None,
      injectedActiveNodeName: Option[NodeName] = None,
      injectedPassiveUserId: Option[UserId] = None)
    : PassiveClusterNode[S] =
      assertThat(!passiveOrWorkingNode.get().exists(_.isLeft))
      val allocated = PassiveClusterNode
        .resource[SyncIO, S](setting, recovered,
          activeNodeName = injectedActiveNodeName getOrElse
            recovered.state.clusterNodeIdToName(setting.activeId).orThrow,
          passiveUserId = injectedPassiveUserId getOrElse
            recovered.state.clusterNodeToUserId(setting.passiveId).orThrow,
          eventIdGenerator, initialFileEventId,
          otherFailedOver, clusterConf, common)
        .toAllocated.run()
      passiveOrWorkingNode := Some(Left(allocated))
      allocated.allocatedThing

    val prepared = recovered.clusterState match
      case Empty =>
        if clusterConf.isPrimary then
          logger.debug(s"Active primary cluster $ownId, no backup node appointed")
          Prepared(
            currentPassiveReplicatedState = IO.none,
            untilRecovered = activationInhibitor.startActive.as(Right(recovered)))
        else
          prepareBackupNodeWithEmptyClusterState()

      case clusterState: HasNodes =>
        if ownId == clusterState.activeId then
          startAsActiveNodeWithBackup
        else
          preparePassiveNode(recovered, clusterState)

    Service.resource:
      for
        currentStateRef <- Ref[IO].of:
          prepared.currentPassiveReplicatedState
            .map(_.toChecked(ClusterNodeIsNotReadyProblem).flatten)
      yield
        ClusterNode(
          prepared, passiveOrWorkingNode, currentStateRef,
          clusterConf,
          eventIdGenerator, eventBus.narrowPublisher, common,
          shuttingDown,
          recovered.extract, actorSystem)

  end resource


  // TODO Provisional fix because it's not easy to restart the recovery
  // ServiceMain catches this exception by its `MainServiceTerminationException` trait !!!
  final class RestartAfterJournalTruncationException
  extends RuntimeException("Restart after journal truncation"),
    MainServiceTerminationException,
    NoStackTrace:

    def termination: ProgramTermination =
      ProgramTermination.Restart


  final case class ClusterWatchConfirmed(
    command: ClusterWatchConfirm,
    result: Checked[Unit])


  private final case class Prepared[S <: ClusterableState[S]](
    currentPassiveReplicatedState: IO[Option[Checked[S]]],
    untilRecovered: IO[Checked[Recovered[S]]],
    expectingStartBackupCommand: Option[Promise[ClusterStartBackupNode]] = None)
