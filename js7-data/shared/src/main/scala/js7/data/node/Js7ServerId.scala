package js7.data.node

import js7.base.annotation.javaApi
import js7.data.cluster.ClusterWatchId
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
  def proxy(name: String): Js7ServerId =
    Proxy(name)

  final case class Proxy(name: String) extends Js7ServerId:
    override def toString = s"Proxy:$name"

  object Proxy:
    def apply(clusterWatchId: ClusterWatchId): Js7ServerId =
      Proxy(clusterWatchId.string)

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
