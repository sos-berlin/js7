package js7.data.event

import cats.syntax.traverse.*
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.checkedSubtype
import js7.data.cluster.ClusterState
import js7.data.cluster.ClusterState.HasNodes
import js7.data.node.{NodeId, NodeName, NodeNameToPassword}

/** A JournaledState with snapshot, JournalState and ClusterState. */
trait ClusterableState[S <: ClusterableState[S]]
extends SnapshotableState[S]:
  this: S =>

  override def companion: ClusterableState.Companion[S]

  def clusterNodeIdToName(nodeId: NodeId): Checked[NodeName]

  def clusterNodeToUserId(nodeId: NodeId): Checked[UserId]

  def toPeerAndAdmission(ownId: NodeId)(using NodeNameToPassword[S])
  : Checked[(NodeId, Admission, ClusterState.HasNodes)] =
    clusterState.checkedSubtype[HasNodes].flatMap: clusterState =>
      val peerId = clusterState.setting.other(ownId)
      clusterNodeToUserAndPassword(ownId).map: peersUserAndPassword =>
        val admission = Admission(clusterState.setting.idToUri(peerId), peersUserAndPassword)
        (peerId, admission, clusterState)

  final def clusterNodeToUserAndPassword(ownId: NodeId)(using NodeNameToPassword[S])
  : Checked[Option[UserAndPassword]] =
    clusterState.checkedSubtype[HasNodes].flatMap: clusterState =>
      val otherNodeId = clusterState.setting.other(ownId)
      clusterNodeToUserAndPassword(ownId, otherNodeId)

  final def clusterNodeToUserAndPassword(ownId: NodeId, peerId: NodeId)
    (using nodeNameToPassword: NodeNameToPassword[S])
  : Checked[Option[UserAndPassword]] =
    for
      nodeName <- clusterNodeIdToName(peerId)
      maybePassword <- nodeNameToPassword(nodeName)
      maybeUserAndPassword <-
        maybePassword.traverse(password =>
          for userId <- clusterNodeToUserId(ownId) yield
            UserAndPassword(userId, password))
    yield
      maybeUserAndPassword


object ClusterableState:
  trait Companion[S <: ClusterableState[S]]
  extends SnapshotableState.Companion[S]:
    implicit final val implicitClusterableStateCompanion: Companion[S] = this

    def empty: S

    def callExpliclitlyAfterAggregateInitialisation = false
