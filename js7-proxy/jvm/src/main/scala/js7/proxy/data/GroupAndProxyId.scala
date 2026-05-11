package js7.proxy.data

import js7.base.annotation.javaApi
import js7.data.node.{Js7ServerGroupId, Js7ServerId}
import js7.data.proxy.ProxyId

final case class GroupAndProxyId(groupId: Js7ServerGroupId.Proxy, proxyId: ProxyId):
  def toGroupAndServerId: (Js7ServerGroupId.Proxy, Js7ServerId.Proxy) =
    groupId -> proxyId.toJs7ServerId


object GroupAndProxyId:

  @javaApi
  def of(serverGroupId: String, proxyId: String) =
    GroupAndProxyId(Js7ServerGroupId.Proxy(serverGroupId), ProxyId(proxyId))
