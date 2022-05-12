package js7.cluster

import akka.util.Timeout
import com.softwaremill.diffx
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.{Checked, Problem}
import js7.base.thread.Futures.syntax._
import js7.base.utils.ScalaUtils.syntax.{RichEitherF, RichThrowable}
import js7.base.utils.SetOnce
import js7.base.web.Uri
import js7.cluster.ClusterConf.ClusterProductName
import js7.cluster.WorkingClusterNode._
import js7.data.Problems.ClusterNodesAlreadyAppointed
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.{ClusterCommand, ClusterSetting, ClusterState}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, SnapshotableState}
import js7.data.node.NodeId
import js7.journal.state.FileStatePersistence
import monix.eval.Task
import monix.execution.Scheduler
import scala.reflect.runtime.universe._

/** A WorkingClusterNode may be in Empty (no cluster) or HasNodes ClusterState.
  *
  * In contrast, ActiveClusterNode requires ClusterState.HasNodes.
  * While in ClusterState.Empty, this WorkingClusterNodes methods do nothing or return an error.
  * Otherwise, WorkingClusterNode forwards the calls to the ActiveClusterState.
  * WorkingClusterNode also starts ActiveClusterNodes after
  * the ClusterNodesAppointed event.
  */
final class WorkingClusterNode[S <: SnapshotableState[S]: SnapshotableState.Companion: diffx.Diff: TypeTag](
  persistence: FileStatePersistence[S],
  common: ClusterCommon,
  clusterConf: ClusterConf)
  (implicit scheduler: Scheduler, journalActorAskTimeout: Timeout)
{
  import clusterConf.ownId

  private val _activeClusterNode = SetOnce[ActiveClusterNode[S]](
    Problem.pure(s"This cluster node '$ownId' is not active now"))
  private val activeClusterNodeTask = Task { _activeClusterNode.checked }
  private val currentClusterState = persistence.clusterState

  def startIfNonEmpty(clusterState: ClusterState, eventId: EventId): Task[Checked[Completed]] =
    clusterState match {
      case ClusterState.Empty => Task.pure(Right(Completed))
      case clusterState: HasNodes =>
        Task(common.licenseChecker.checkLicense(ClusterProductName))
          .flatMapT(_ =>
            startActiveClusterNode(clusterState, eventId))
    }

  def close(): Unit =
    for (o <- _activeClusterNode) {
      o.stop.runToFuture.onFailure { case t =>
        logger.warn(s"WorkingClusterNode.stop => ${t.toStringWithCauses}", t.nullIfNoStackTrace)
      }
    }

  def stop: Task[Completed] =
    _activeClusterNode.toOption.fold(Task.completed)(_.stop)

  def beforeJournalingStarts: Task[Checked[Completed]] =
    _activeClusterNode.toOption match {
      case None => Task.pure(Right(Completed))
      case Some(o) => o.beforeJournalingStarts
    }

  def afterJournalingStarted: Task[Checked[Completed]] =
    automaticallyAppointConfiguredBackupNode >>
      (_activeClusterNode.toOption match {
        case None => Task.pure(Right(Completed))
        case Some(o) => o.onRestartActiveNode
      })

  def appointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId, clusterWatches: Seq[ClusterSetting.Watch])
  : Task[Checked[Completed]] =
    Task.pure(ClusterSetting.checked(idToUri, activeId, clusterWatches, clusterConf.timing))
      .flatMapT(appointNodes)

  private def automaticallyAppointConfiguredBackupNode: Task[Checked[Completed]] =
    Task.defer {
      clusterConf.maybeClusterSetting match {
        case None => Task.pure(Right(Completed))
        case Some(setting) =>
          persistence.clusterState.flatMap {
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

  private def appointNodes(setting: ClusterSetting): Task[Checked[Completed]] =
    currentClusterState.flatMap {
      case ClusterState.Empty =>
        Task(common.licenseChecker.checkLicense(ClusterProductName))
          .flatMapT(_ =>
            persistence.persistKeyedEvent(NoKey <-: ClusterNodesAppointed(setting))
              .flatMapT { case (_, state) =>
                state.clusterState match {
                  case clusterState: HasNodes =>
                    startActiveClusterNode(clusterState, state.eventId)
                  case clusterState => Task.pure(Left(Problem.pure(
                    s"Unexpected ClusterState $clusterState after ClusterNodesAppointed")))
                }
              })

      case _: HasNodes =>
        activeClusterNodeTask
          .flatMapT(_.appointNodes(setting))
    }

  private def startActiveClusterNode(clusterState: HasNodes, eventId: EventId): Task[Checked[Completed]] =
    Task.defer {
      val activeClusterNode = new ActiveClusterNode(clusterState, persistence, common, clusterConf)
      if (_activeClusterNode.trySet(activeClusterNode))
        activeClusterNode.start(eventId)
      else
        Task.pure(Left(Problem.pure("ActiveClusterNode has already been started")))
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
    _activeClusterNode.toOption match {
      case None => Task.pure(Right(Completed))
      case Some(o) => o.shutDownThisNode
    }

  def isActive = _activeClusterNode.isDefined
}

object WorkingClusterNode
{
  private val logger = Logger(getClass)
}
