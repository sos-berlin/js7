package js7.cluster.watch

import cats.syntax.option.*
import js7.base.catsutils.SyncDeadline
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.functionCallToString
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{HasNodes, PassiveLost}
import js7.data.cluster.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeLossNotConfirmedProblem, ClusterWatchEventMismatchProblem, ClusterWatchInactiveNodeProblem, ClusterWatchNotAskingProblem}
import js7.data.cluster.{ClusterState, ClusterWatchAskNodeLoss, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchCommitNodeLoss, ClusterWatchNonCommitRequest, Confirmer}

/** Provide the State type hierarchy: Untaught, Normal, AskingNodeLoss. */
private transparent trait ClusterWatchStateMixin:

  protected val requireManualNodeLossConfirmation: Boolean
  protected val label: String

  private val logger = Logger.withPrefix[this.type](label)

  protected sealed trait State:
    def ifNormal: Option[Normal] =
      this.ifSubtype[Normal]


  protected sealed trait NormalOrUntaught extends State:
    final def fold[A](whenUntaught: => A)(whenNormal: Normal => A): A =
      this.ifNormal.fold(whenUntaught)(whenNormal)

  protected type Untaught = Untaught.type
  protected case object Untaught extends NormalOrUntaught:
    def savedNormal = Untaught


  protected sealed trait NormalOrAsking extends State:
    def savedNormal: NormalOrUntaught

    override final def ifNormal: Option[Normal] =
      savedNormal.ifSubtype[Normal]


  /** ClusterWatch is taught (knows the ClusterState). */
  protected final case class Normal(
    clusterState: HasNodes,
    lastHeartbeat: SyncDeadline,
    requireManualNodeLossConfirmation: Boolean)
  extends NormalOrUntaught, NormalOrAsking:

    def savedNormal: Normal = this

    def setLastHeartbeat(now: SyncDeadline.Now): Normal =
      copy(lastHeartbeat = now)

    def processRequest(
      request: ClusterWatchNonCommitRequest,
      ifManuallyConfirmed: ClusterNodeLostEvent => Option[Confirmer],
      opString: => String)
      (using now: SyncDeadline.Now)
    : Checked[Option[Confirmer]] =
      if
        (request.isSubtypeOf[ClusterWatchCheckState] || request.isSubtypeOf[ClusterWatchCheckEvent])
          && request.clusterState == clusterState
      then
        if request.maybeEvent.nonEmpty then
          logger.debug:
            s"${request.from}: 🪱 Ignore probably duplicate event for already reached clusterState=$clusterState"
        Right(None)
      else
        def clusterWatchInactiveNodeProblem =
          ClusterWatchInactiveNodeProblem(request.from, clusterState, lastHeartbeat.elapsed, opString)

        request.maybeEvent.match
          case Some(_: ClusterSwitchedOver) =>
            // ClusterSwitchedOver is applied by each node and is considered reliable
            Right(())

          case Some(ClusterFailedOver(failedActiveId, _, _)) =>
            clusterState match
              case PassiveLost(setting) if setting.activeId == failedActiveId =>
                Left(ClusterFailOverWhilePassiveLostProblem)

              case clusterState =>
                (request.from == clusterState.passiveId && !isLastHeartbeatStillValid) !!
                  clusterWatchInactiveNodeProblem
          case _ =>
            (request.from == clusterState.activeId) !! clusterWatchInactiveNodeProblem
        .flatMap: _ =>
          clusterState.applyEvents(request.maybeEvent) match
            case Left(problem) =>
              logger.warn(s"${request.from}: $problem")
              Left(ClusterWatchEventMismatchProblem(
                request.maybeEvent, clusterState, reportedClusterState = request.clusterState))

            case Right(updatedClusterState) =>
              for event <- request.maybeEvent do logger.info(s"${request.from} $event")
              val confirmer = request.maybeEvent match
                case Some(event: ClusterNodeLostEvent) => ifManuallyConfirmed(event)
                case _ => None
              request.maybeEvent match
                case Some(event: ClusterNodeLostEvent)
                  if requireManualNodeLossConfirmation && !confirmer.isDefined =>
                  Left(ClusterNodeLossNotConfirmedProblem(request.from, event))
                case _ =>
                  if updatedClusterState == request.clusterState then
                    logger.info(s"${request.from} changes ClusterState to ${request.clusterState}")
                    Right(confirmer)
                  else
                    // The node may have died just between sending the event to
                    // ClusterWatch and persisting it. Then we have a different state.
                    val previouslyActive = clusterState.activeId.string
                    logger.warn(s"${request.from} forced ClusterState to ${request.clusterState} " +
                      s"maybe because the last heartbeat of the sofar active $previouslyActive " +
                      s"is too long ago (${lastHeartbeat.elapsed.pretty})")
                    Right(confirmer)
        .onProblem: problem =>
          logger.warn(s"${request.from}: $problem")

    private def isLastHeartbeatStillValid(using SyncDeadline.Now) =
      (lastHeartbeat + clusterState.timing.clusterWatchHeartbeatValidDuration).hasTimeLeft

    override def toString =
      functionCallToString("Normal",
        clusterState.toShortString.some,
        s"lastHeartbeat=$lastHeartbeat".some,
        requireManualNodeLossConfirmation ? "requireManualNodeLossConfirmation")


  /** A Cluster node is asking the ClusterWatch about a node loss. */
  protected final case class AskingNodeLoss(
    clusterState: ClusterState.HasNodes,
    lastHeartbeat: SyncDeadline,
    requireManualNodeLossConfirmation: Boolean,
    request: ClusterWatchAskNodeLoss,
    askedClusterState: ClusterState.IsNodeLost,
    holdUntil: SyncDeadline,
    savedNormal: NormalOrUntaught)
  extends NormalOrAsking:

    def restoreNormal: State =
      savedNormal

    def processRequest(
      commit: ClusterWatchCommitNodeLoss,
      confirmer: Option[Confirmer],
      opString: => String)
      (using now: SyncDeadline.Now)
    : Checked[Option[Confirmer]] =
      def clusterWatchInactiveNodeProblem =
        ClusterWatchInactiveNodeProblem(request.from, clusterState, lastHeartbeat.elapsed, opString)

      matches(commit).flatMap: _ =>
        logger.info(s"${commit.from} ${commit.event}")
        if requireManualNodeLossConfirmation && !confirmer.isDefined then
          Left(ClusterNodeLossNotConfirmedProblem(commit.from, commit.event))
        else
          logger.info(s"${request.from} changes ClusterState to ${request.clusterState}")
          Right(confirmer)
      .onProblem: problem =>
        logger.warn(s"${request.from}: $problem")

    private def matches(commit: ClusterWatchCommitNodeLoss): Checked[Unit] =
      if commit.from == request.from
        && commit.clusterState == request.clusterState
        && commit.event == request.event
        && commit.forceWhenUntaught == request.forceWhenUntaught
      then
        Checked.unit
      else
        Left(ClusterWatchNotAskingProblem)

    override def toString =
      functionCallToString("AskingNodeLoss",
        clusterState.toShortString.some,
        s"lastHeartbeat=$lastHeartbeat".some,
        requireManualNodeLossConfirmation ? "requireManualNodeLossConfirmation",
        request.some,
        s"askedClusterState=${askedClusterState.toShortString}".some,
        s"holdUntil=$holdUntil".some,
        s"savedNormal=$savedNormal".some)
