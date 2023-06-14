package js7.cluster

import akka.actor.ActorRefFactory
import cats.effect.Resource
import com.softwaremill.diffx
import izumi.reflect.Tag
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichEitherF, RichOption}
import js7.base.utils.{Allocated, AsyncLock, SetOnce}
import js7.base.web.Uri
import js7.cluster.WorkingClusterNode.*
import js7.data.Problems.{ClusterNodeIsNotActiveProblem, ClusterNodesAlreadyAppointed}
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterCommand, ClusterSetting, ClusterState}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, EventId, SnapshotableState, Stamped}
import js7.data.node.NodeId
import js7.journal.EventIdGenerator
import js7.journal.recover.Recovered
import js7.journal.state.FileJournal
import monix.eval.Task
import monix.execution.Scheduler

/** A WorkingClusterNode may be in Empty (no cluster) or HasNodes ClusterState.
  *
  * In contrast, ActiveClusterNode requires ClusterState.HasNodes.
  * While in ClusterState.Empty, this WorkingClusterNodes methods do nothing or return an error.
  * Otherwise, WorkingClusterNode forwards the calls to the ActiveClusterState.
  * WorkingClusterNode also starts ActiveClusterNodes after
  * the ClusterNodesAppointed event.
  */
final class WorkingClusterNode[
  S <: SnapshotableState[S]: SnapshotableState.Companion: diffx.Diff: Tag
] private(
  val failedNodeId: Option[NodeId],
  val journalAllocated: Allocated[Task, FileJournal[S]],
  common: ClusterCommon,
  clusterConf: ClusterConf)
  (implicit scheduler: Scheduler)
//TODO extends Service
{
  val journal: FileJournal[S] = journalAllocated.allocatedThing
  private val _activeClusterNode = SetOnce.undefined[ActiveClusterNode[S]](
    "ActiveClusterNode", ClusterNodeIsNotActiveProblem)
  private val activeClusterNodeTask = Task { _activeClusterNode.checked }
  private val currentClusterState = journal.clusterState
  private val appointNodesLock = AsyncLock()

  def start(clusterState: ClusterState, eventId: EventId): Task[Checked[Unit]] =
    clusterState match {
      case ClusterState.Empty => Task.pure(Right(Completed))
      case _: HasNodes =>
        common.requireValidLicense
          .flatMapT(_ =>
            startActiveClusterNode(eventId))
    }

  def stop: Task[Unit] =
    Task.defer {
      _activeClusterNode.toOption.fold(Task.unit)(_.stop)
    }

  def beforeJournalingStarts: Task[Checked[Unit]] =
    _activeClusterNode.toOption match {
      case None => Task.right(Completed)
      case Some(o) => o.beforeJournalingStarts
    }

  def afterJournalingStarted: Task[Checked[Completed]] =
    automaticallyAppointConfiguredBackupNode.flatMapT(_ =>
      (_activeClusterNode.toOption match {
        case None => Task.right(Completed)
        case Some(o) => o.onRestartActiveNode
      }))

  def appointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId): Task[Checked[Unit]] =
    Task(ClusterSetting
      .checked(
        idToUri, activeId, clusterConf.timing,
        clusterWatchId = None)
    ).flatMapT(appointNodes(_))

  private def automaticallyAppointConfiguredBackupNode: Task[Checked[Unit]] =
    Task.defer {
      clusterConf.maybeClusterSetting match {
        case None => Task.pure(Right(Completed))
        case Some(setting) =>
          journal.clusterState.flatMap {
            case _: ClusterState.HasNodes => Task.pure(Right(Completed))
            case ClusterState.Empty =>
              appointNodes(setting)
                .onErrorHandle(t => Left(Problem.fromThrowable(t)))  // We want only to log the exception
                .map {
                  case Left(ClusterNodesAlreadyAppointed) =>
                    Right(Completed)

                  case Left(problem) =>
                    Left(problem.withPrefix(
                      "Configured cluster node appointment failed, maybe due to failed access to ClusterWatch:"))

                  case Right(completed) =>
                    Right(completed)
                }
          }
      }
    }

  private def appointNodes(setting: ClusterSetting): Task[Checked[Unit]] =
    logger.debugTask(appointNodesLock.lock(
      currentClusterState.flatMap {
        case ClusterState.Empty =>
          journal.persistKeyedEvent(NoKey <-: ClusterNodesAppointed(setting))
            .flatMapT { case (_, state) =>
              state.clusterState match {
                case clusterState: HasNodes =>
                  startActiveClusterNode(state.eventId)
                case clusterState => Task.left(Problem.pure(
                  s"Unexpected ClusterState $clusterState after ClusterNodesAppointed"))
              }
            }

        case _: HasNodes =>
          common.requireValidLicense
            .flatMapT(_ => activeClusterNodeTask)
            .flatMapT(_.appointNodes(setting))
      }))

  private def startActiveClusterNode(eventId: EventId): Task[Checked[Unit]] =
    Task.defer {
      val activeClusterNode = new ActiveClusterNode(journal, common, clusterConf)
      if (_activeClusterNode.trySet(activeClusterNode))
        activeClusterNode.start(eventId)
      else
        Task.left(Problem.pure("ActiveClusterNode has already been started"))
    }

  def executeClusterWatchConfirm(cmd: ClusterWatchConfirm): Task[Checked[Unit]] =
    Task.defer {
      _activeClusterNode.toOption.fold_(Task.left(ClusterNodeIsNotActiveProblem),
        _.executeClusterWatchConfirm(cmd))
    }

  def onTerminatedUnexpectedly: Task[Checked[Completed]] =
    _activeClusterNode.task
      .flatMap(_.onTerminatedUnexpectedly)

  def switchOver: Task[Checked[Completed]] =
    activeClusterNodeTask
      .flatMapT(_.switchOver)

  def executeCommand(command: ClusterCommand): Task[Checked[ClusterCommand.Response]] =
    activeClusterNodeTask
      .flatMapT(_.executeCommand(command))

  def shutDownThisNode: Task[Checked[Completed]] =
    Task.defer {
      _activeClusterNode.toOption match {
        case None => Task.pure(Right(Completed))
        case Some(o) => o.shutDownThisNode
      }
    }

  def isActive = _activeClusterNode.isDefined
}

object WorkingClusterNode
{
  private val logger = Logger(getClass)

  private[cluster]
  def resource[S <: SnapshotableState[S]: SnapshotableState.Companion: diffx.Diff: Tag](
    recovered: Recovered[S],
    common: ClusterCommon,
    clusterConf: ClusterConf,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator,
    keyedEventBus: EventPublisher[Stamped[AnyKeyedEvent]] = new StandardEventBus)
    (implicit scheduler: Scheduler, actorRefFactory: ActorRefFactory, timeout: akka.util.Timeout)
  : Resource[Task, WorkingClusterNode[S]] =
    for {
      _ <- Resource.eval(Task.unless(recovered.clusterState == ClusterState.Empty)(
        common.requireValidLicense.map(_.orThrow)))
      journalAllocated <- Resource.eval(FileJournal
        .resource(recovered, clusterConf.journalConf, eventIdGenerator, keyedEventBus)
        .toAllocated/* ControllerOrderKeeper and AgentOrderKeeper both require Allocated*/)
      workingClusterNode <- Resource.make(
        acquire = Task.defer {
          val w = new WorkingClusterNode(recovered.failedNodeId, journalAllocated, common, clusterConf)
          w.start(recovered.clusterState, recovered.eventId).as(w)
        })(
        release = _.stop)
    } yield workingClusterNode
}
