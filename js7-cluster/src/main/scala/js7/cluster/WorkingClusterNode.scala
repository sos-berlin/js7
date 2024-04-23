package js7.cluster

import cats.effect.{IO, Resource, ResourceIO}
import cats.effect.unsafe.IORuntime
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichEitherF, RichOption}
import js7.base.utils.{Allocated, AsyncLock, SetOnce}
import js7.base.web.Uri
import js7.cluster.WorkingClusterNode.*
import js7.data.Problems.{ClusterNodeIsNotActiveProblem, ClusterNodesAlreadyAppointed, ClusterSettingNotUpdatable}
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterCommand, ClusterSetting, ClusterState}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, ClusterableState, EventId, NoKeyEvent, Stamped}
import js7.data.item.BasicItemEvent.ItemAttachedToMe
import js7.data.node.{NodeId, NodeNameToPassword}
import js7.journal.EventIdGenerator
import js7.journal.recover.Recovered
import js7.journal.state.FileJournal
import org.apache.pekko
import org.apache.pekko.actor.ActorRefFactory

/** A WorkingClusterNode may be in Empty (no cluster) or HasNodes ClusterState.
  *
  * In contrast, ActiveClusterNode requires ClusterState.HasNodes.
  * While in ClusterState.Empty, this WorkingClusterNodes methods do nothing or return an error.
  * Otherwise, WorkingClusterNode forwards the calls to the ActiveClusterState.
  * WorkingClusterNode also starts ActiveClusterNodes after
  * the ClusterNodesAppointed event.
  */
final class WorkingClusterNode[
  S <: ClusterableState[S]: ClusterableState.Companion/*: diffx.Diff*/: Tag
] private(
  val failedNodeId: Option[NodeId],
  val journalAllocated: Allocated[IO, FileJournal[S]],
  common: ClusterCommon,
  clusterConf: ClusterConf)
  (implicit
    nodeNameToPassword: NodeNameToPassword[S],
    ioRuntime: IORuntime):
//TODO extends Service

  val journal: FileJournal[S] = journalAllocated.allocatedThing
  private val _activeClusterNode = SetOnce.undefined[ActiveClusterNode[S]](
    "ActiveClusterNode", ClusterNodeIsNotActiveProblem)
  private val activeClusterNodeIO = IO { _activeClusterNode.checked }
  private val currentClusterState = journal.clusterState
  private val appointNodesLock = AsyncLock()

  def start(clusterState: ClusterState, eventId: EventId): IO[Checked[Unit]] =
    clusterState match
      case ClusterState.Empty => IO.right(Completed)
      case clusterState: HasNodes =>
        common.requireValidLicense
          .flatMapT(_ =>
            startActiveClusterNode(clusterState, eventId))

  def stop: IO[Unit] =
    IO.defer:
      _activeClusterNode.toOption.fold(IO.unit)(_.stop)

  def beforeJournalingStarts: IO[Checked[Unit]] =
    _activeClusterNode.toOption match
      case None => IO.right(Completed)
      case Some(o) => o.beforeJournalingStarts

  def afterJournalingStarted: IO[Checked[Completed]] =
    automaticallyAppointConfiguredBackupNode.flatMapT(_ =>
      _activeClusterNode.toOption match {
        case None => IO.right(Completed)
        case Some(o) => o.onRestartActiveNode
      })

  def appointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId,
    extraEvent: Option[ItemAttachedToMe] = None)
  : IO[Checked[Unit]] =
    IO(
      ClusterSetting.checked(
        idToUri, activeId, clusterConf.timing,
        clusterWatchId = None)
    ).flatMapT(
      appointNodes2(_, extraEvent))

  private def automaticallyAppointConfiguredBackupNode: IO[Checked[Unit]] =
    IO.defer:
      clusterConf.maybeClusterSetting match
        case None => IO.right(Completed)
        case Some(setting) =>
          journal.clusterState.flatMap:
            case _: ClusterState.HasNodes => IO.right(Completed)
            case ClusterState.Empty =>
              appointNodes2(setting)
                .recover(t => Left(Problem.fromThrowable(t)))  // We want only to log the exception
                .map:
                  case Left(ClusterNodesAlreadyAppointed) =>
                    Right(Completed)

                  case Left(problem) =>
                    Left(problem.withPrefix(
                      "Configured cluster node appointment failed, maybe due to failed access to ClusterWatch:"))

                  case Right(completed) =>
                    Right(completed)

  private def appointNodes2(setting: ClusterSetting, extraEvent: Option[ItemAttachedToMe] = None)
  : IO[Checked[Unit]] =
    logger.debugIO(appointNodesLock.lock(
      currentClusterState.flatMap {
        case ClusterState.Empty =>
          journal
            .persistTransaction[NoKeyEvent](NoKey)(_ =>
              Right(extraEvent.toList ::: List(ClusterNodesAppointed(setting))))
            .flatMapT { case (_, state) =>
              state.clusterState match {
                case clusterState: HasNodes =>
                  startActiveClusterNode(clusterState, state.eventId)
                case clusterState => IO.left(Problem.pure(
                  s"Unexpected ClusterState $clusterState after ClusterNodesAppointed"))
              }
            }

        case clusterState @ HasNodes(current) =>
          if setting != current.copy(clusterWatchId = None).withPassiveUri(setting.passiveUri) then
            IO.left(ClusterSettingNotUpdatable(clusterState))
          else if setting.passiveUri == current.passiveUri then
            extraEvent.fold(IO.pure(Checked.unit))(extraEvent =>
              journal.persistKeyedEvent(extraEvent).rightAs(()))
          else
            activeClusterNodeIO
              .flatMapT(_.changePassiveUri(setting.passiveUri, extraEvent))
      }))

  private def startActiveClusterNode(clusterState: HasNodes, eventId: EventId): IO[Checked[Unit]] =
    logger.traceIO:
      val passiveNodeId = clusterState.setting.other(clusterState.activeId)
      journal.state
        .map(_.clusterNodeToUserAndPassword(
          ourNodeId = clusterState.activeId,
          otherNodeId = passiveNodeId))
        .flatMapT(passiveNodeUserAndPassword =>
          IO.defer {
            val activeClusterNode =
              new ActiveClusterNode(journal, passiveNodeUserAndPassword, common, clusterConf)
            if _activeClusterNode.trySet(activeClusterNode) then
              activeClusterNode.start(eventId)
            else
              IO.left(Problem.pure("ActiveClusterNode has already been started"))
          })

  def executeClusterWatchConfirm(cmd: ClusterWatchConfirm): IO[Checked[Unit]] =
    IO.defer:
      _activeClusterNode.toOption.fold_(IO.left(ClusterNodeIsNotActiveProblem),
        _.executeClusterWatchConfirm(cmd))

  def onTerminatedUnexpectedly: IO[Checked[Completed]] =
    _activeClusterNode.io
      .flatMap(_.onTerminatedUnexpectedly)

  def switchOver: IO[Checked[Completed]] =
    activeClusterNodeIO
      .flatMapT(_.switchOver)

  def executeCommand(command: ClusterCommand): IO[Checked[ClusterCommand.Response]] =
    activeClusterNodeIO
      .flatMapT(_.executeCommand(command))

  def shutDownThisNode: IO[Checked[Completed]] =
    logger.debugIO(IO.defer:
      _activeClusterNode.toOption match
        case None => IO.right(Completed)
        case Some(o) => o.shutDownThisNode)

  def isActive: Boolean =
    _activeClusterNode.isDefined


object WorkingClusterNode:
  private val logger = Logger[this.type]

  private[cluster]
  def resource[S <: ClusterableState[S]: ClusterableState.Companion/*: diffx.Diff*/: Tag](
    recovered: Recovered[S],
    common: ClusterCommon,
    clusterConf: ClusterConf,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator,
    keyedEventBus: EventPublisher[Stamped[AnyKeyedEvent]])
    (implicit
      nodeNameToPassword: NodeNameToPassword[S],
      ioRuntime: IORuntime,
      actorRefFactory: ActorRefFactory,
      timeout: pekko.util.Timeout)
  : ResourceIO[WorkingClusterNode[S]] =
    for
      _ <- Resource.eval(IO.unlessA(recovered.clusterState == ClusterState.Empty)(
        common.requireValidLicense.map(_.orThrow)))
      journalAllocated <- Resource.eval(FileJournal
        .resource(recovered, clusterConf.journalConf, eventIdGenerator, keyedEventBus)
        // Not compilable with Scala 3.3.1: .toAllocated
        .toLabeledAllocated(label = s"FileJournal[${implicitly[Tag[S]].tag.shortName}]")
        /* ControllerOrderKeeper and AgentOrderKeeper both require Allocated*/)
      workingClusterNode <- Resource.make(
        acquire = IO.defer {
          val w = new WorkingClusterNode(recovered.failedNodeId, journalAllocated, common, clusterConf)
          w.start(recovered.clusterState, recovered.eventId).as(w)
        })(
        release = _.stop)
    yield workingClusterNode
