package js7.data.proxy

import js7.base.generic.GenericString
import js7.data.cluster.ClusterWatchId
import js7.data.node.Js7ServerId

final case class ProxyId(string: String) extends GenericString:

  def toJs7ServerId: Js7ServerId.Proxy =
    Js7ServerId.Proxy(this)

  override def toString = s"Proxy:$string"


object ProxyId:
  def fromClusterWatchId(clusterWatchId: ClusterWatchId): ProxyId =
    new ProxyId(clusterWatchId.string)
