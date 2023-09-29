package js7.data.event

import cats.syntax.traverse.*
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.problem.Checked
import js7.data.node.{NodeId, NodeName, NodeNameToPassword}

/** A JournaledState with snapshot, JournalState and ClusterState. */
trait ClusterableState[S <: ClusterableState[S]]
extends SnapshotableState[S]:
  this: S =>

  override def companion: ClusterableState.Companion[S]

  def clusterNodeIdToName(nodeId: NodeId): Checked[NodeName]

  def clusterNodeToUserId(nodeId: NodeId): Checked[UserId]

  final def clusterNodeToUserAndPassword(ourNodeId: NodeId, otherNodeId: NodeId)
    (implicit nodeNameToPassword: NodeNameToPassword[S])
  : Checked[Option[UserAndPassword]] =
    for
      nodeName <- clusterNodeIdToName(otherNodeId)
      maybePassword <- nodeNameToPassword(nodeName)
      maybeUserAndPassword <-
        maybePassword.traverse(password =>
          for userId <- clusterNodeToUserId(ourNodeId) yield
            UserAndPassword(userId, password))
    yield maybeUserAndPassword

object ClusterableState:
  trait Companion[S <: ClusterableState[S]]
  extends SnapshotableState.Companion[S]:
    implicit final val implicitClusterableStateCompanion: Companion[S] = this

    def empty: S
