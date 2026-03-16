package js7.data.node

import js7.base.annotation.javaApi
import js7.data.subagent.SubagentId

sealed trait EngineServerId

object EngineServerId:

  @javaApi
  val primaryController: EngineServerId =
    Controller.Primary

  @javaApi
  val backupController: EngineServerId =
    Controller.Backup

  @javaApi
  def subagent(subagentId: SubagentId): EngineServerId =
    Subagent(subagentId)


  sealed trait Controller extends EngineServerId:
    def nodeId: NodeId

  object Controller:
    case object Primary extends Controller:
      val nodeId = NodeId.primary
      override def toString = "PrimaryController"

    case object Backup extends Controller:
      val nodeId = NodeId.backup
      override def toString = "BackupController"

    @javaApi
    val controllerPrimary: Controller = Primary

    @javaApi
    val controllerBackup: Controller = Backup


  final case class Subagent(subagentId: SubagentId) extends EngineServerId:
    override def toString = subagentId.toString

  object Subagent:
    @javaApi
    def of(subagentId: SubagentId): Subagent =
      Subagent(subagentId)
