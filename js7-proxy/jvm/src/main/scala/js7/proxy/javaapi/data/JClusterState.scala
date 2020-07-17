package js7.proxy.javaapi.data

import js7.data.cluster.ClusterState
import js7.data.node.NodeId
import scala.jdk.CollectionConverters._

sealed trait JClusterState
extends JJsonable[JClusterState]
{
  protected type Underlying = ClusterState

  def companion = JClusterState
}

object JClusterState extends JJsonable.Companion[JClusterState]
{
  private type Id = NodeId

  def apply(clusterState: ClusterState): JClusterState =
    clusterState match {
      case ClusterState.Empty => Empty
      case o: ClusterState.NodesAppointed => NodesAppointed(o)
      case o: ClusterState.PreparedToBeCoupled => PreparedToBeCoupled(o)
      case o: ClusterState.Coupled => Coupled(o)
      case o: ClusterState.CoupledActiveShutDown => CoupledActiveShutDown(o)
      case o: ClusterState.PassiveLost => PassiveLost(o)
      case o: ClusterState.SwitchedOver => SwitchedOver(o)
      case o: ClusterState.FailedOver => FailedOver(o)
    }

  sealed trait Empty extends JClusterState

  case object Empty extends Empty {
    val underlying = ClusterState.Empty
  }

  val empty = Empty

  sealed trait HasNodes extends JClusterState
  {
    this: Product =>

    def underlying: ClusterState.HasNodes

    def idToUri = underlying.idToUri.asJava
    def activeId = underlying.activeId
    final def isNonEmptyActive(id: Id) = underlying.isNonEmptyActive(id)
    final def passiveId = underlying.passiveId
    final def passiveUri = underlying.passiveUri
  }

  sealed trait CoupledOrDecoupled extends HasNodes {
    this: Product =>
  }

  sealed trait Decoupled extends CoupledOrDecoupled {
    this: Product =>
  }

  final case class NodesAppointed(underlying: ClusterState.NodesAppointed)
  extends Decoupled

  final case class PreparedToBeCoupled(underlying: ClusterState.PreparedToBeCoupled)
  extends HasNodes

  final case class Coupled(underlying: ClusterState.Coupled)
  extends CoupledOrDecoupled

  final case class CoupledActiveShutDown(underlying: ClusterState.CoupledActiveShutDown)
  extends Decoupled

  final case class PassiveLost(underlying: ClusterState.PassiveLost)
  extends Decoupled

  final case class SwitchedOver(underlying: ClusterState.SwitchedOver)
  extends Decoupled

  final case class FailedOver(underlying: ClusterState.FailedOver)
  extends Decoupled

  def jsonEncoder = ClusterState.jsonCodec
  def jsonDecoder = ClusterState.jsonCodec
}
