package js7.data_for_java.cluster

import js7.base.web.Uri
import js7.data.cluster.ClusterState
import js7.data.node.NodeId
import js7.data_for_java.common.JJsonable
import scala.jdk.CollectionConverters.*

sealed trait JClusterState
extends JJsonable[JClusterState]
{
  type AsScala = ClusterState

  def companion = JClusterState
}

object JClusterState extends JJsonable.Companion[JClusterState]
{
  def apply(clusterState: ClusterState): JClusterState =
    clusterState match {
      case ClusterState.Empty => Empty
      case o: ClusterState.NodesAppointed => NodesAppointed(o)
      case o: ClusterState.PreparedToBeCoupled => PreparedToBeCoupled(o)
      case o: ClusterState.Coupled => Coupled(o)
      case o: ClusterState.ActiveShutDown => ActiveShutDown(o)
      case o: ClusterState.PassiveLost => PassiveLost(o)
      case o: ClusterState.SwitchedOver => SwitchedOver(o)
      case o: ClusterState.FailedOver => FailedOver(o)
    }

  sealed trait Empty extends JClusterState

  case object Empty extends Empty {
    val asScala = ClusterState.Empty
  }

  val empty = Empty

  sealed trait HasNodes extends JClusterState
  {
    this: Product =>

    def asScala: ClusterState.HasNodes

    def idToUri: java.util.Map[NodeId, Uri] =
      asScala.idToUri.asJava

    def activeId: NodeId =
      asScala.activeId

    final def isNonEmptyActive(id: NodeId) =
      asScala.isNonEmptyActive(id)

    final def isEmptyOrActive(id: NodeId) =
      asScala.isEmptyOrActive(id)

    final def passiveId: NodeId =
      asScala.passiveId

    final def passiveUri: Uri =
      asScala.passiveUri
  }

  sealed trait CoupledOrDecoupled extends HasNodes {
    this: Product =>
  }

  sealed trait Decoupled extends CoupledOrDecoupled {
    this: Product =>
  }

  final case class NodesAppointed(asScala: ClusterState.NodesAppointed)
  extends Decoupled

  final case class PreparedToBeCoupled(asScala: ClusterState.PreparedToBeCoupled)
  extends HasNodes

  final case class Coupled(asScala: ClusterState.Coupled)
  extends CoupledOrDecoupled

  final case class ActiveShutDown(asScala: ClusterState.ActiveShutDown)
  extends Decoupled

  final case class PassiveLost(asScala: ClusterState.PassiveLost)
  extends Decoupled

  final case class SwitchedOver(asScala: ClusterState.SwitchedOver)
  extends Decoupled

  final case class FailedOver(asScala: ClusterState.FailedOver)
  extends Decoupled

  protected def jsonEncoder = ClusterState.jsonCodec
  protected def jsonDecoder = ClusterState.jsonCodec
}
