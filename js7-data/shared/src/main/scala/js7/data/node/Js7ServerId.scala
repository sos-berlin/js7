package js7.data.node

import js7.base.annotation.javaApi
import js7.data.proxy.ProxyId
import js7.data.subagent.SubagentId
import scala.annotation.static

sealed trait Js7ServerId

object Js7ServerId:

  @javaApi @static
  val primaryController: Js7ServerId =
    Controller.Primary

  @javaApi @static
  val backupController: Js7ServerId =
    Controller.Backup

  @javaApi
  def subagent(subagentId: SubagentId): Js7ServerId =
    Subagent(subagentId)

  @javaApi
  def proxy(proxyId: ProxyId): Js7ServerId =
    Proxy(proxyId)

  final case class Proxy(proxyId: ProxyId) extends Js7ServerId:
    override def toString = proxyId.toString

  type Provider = Provider.type
  case object Provider extends Js7ServerId


  sealed trait Controller extends Js7ServerId:
    def nodeId: NodeId

  object Controller:
    case object Primary extends Controller:
      val nodeId = NodeId.primary
      override def toString = "Controller/primary"

    case object Backup extends Controller:
      val nodeId = NodeId.backup
      override def toString = "Controller/backup"


  final case class Subagent(subagentId: SubagentId) extends Js7ServerId:
    override def toString = subagentId.toString

  object Subagent:
    @javaApi
    def of(subagentId: SubagentId): Subagent =
      Subagent(subagentId)
