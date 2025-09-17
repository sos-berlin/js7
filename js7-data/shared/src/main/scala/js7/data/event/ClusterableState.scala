package js7.data.event

import cats.syntax.traverse.*
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichOrNull
import js7.data.cluster.ClusterState.HasNodes
import js7.data.node.{NodeId, NodeName, NodeNameToPassword}

/** A JournaledState with snapshot, JournalState and ClusterState. */
trait ClusterableState[S <: ClusterableState[S]]
extends SnapshotableState[S]:
  this: S =>

  override def companion: ClusterableState.Companion[S]

  def clusterNodeIdToName(nodeId: NodeId): Checked[NodeName]

  def clusterNodeToUserId(nodeId: NodeId): Checked[UserId]

  final def clusterNodeToUserAndPassword(ourNodeId: NodeId)(using NodeNameToPassword[S])
  : Checked[Option[UserAndPassword]] =
    clusterState.checkedCast[HasNodes].flatMap: clusterState =>
      val otherNodeId = clusterState.setting.other(ourNodeId)
      clusterNodeToUserAndPassword(ourNodeId, otherNodeId)

  final def clusterNodeToUserAndPassword(ourNodeId: NodeId, otherNodeId: NodeId)
    (using nodeNameToPassword: NodeNameToPassword[S])
  : Checked[Option[UserAndPassword]] =
    for
      nodeName <- clusterNodeIdToName(otherNodeId)
      maybePassword <- nodeNameToPassword(nodeName)
      maybeUserAndPassword <-
        maybePassword.traverse(password =>
          for userId <- clusterNodeToUserId(ourNodeId) yield
            UserAndPassword(userId, password))
    yield
      maybeUserAndPassword


object ClusterableState:
  trait Companion[S <: ClusterableState[S]]
  extends SnapshotableState.Companion[S]:
    implicit final val implicitClusterableStateCompanion: Companion[S] = this

    def empty: S

    def callExpliclitlyAfterAggregateInitialisation = false
