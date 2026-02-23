package js7.data.node

import js7.base.annotation.javaApi
import js7.data.subagent.SubagentId

sealed trait EngineServerId

object EngineServerId:

  @javaApi
  val controllerPrimary: EngineServerId =
    ControllerServerId.ControllerPrimary

  @javaApi
  val controllerBackup: EngineServerId =
    ControllerServerId.ControllerBackup

  @javaApi
  def subagent(subagentId: SubagentId): EngineServerId =
    SubagentServerId(subagentId)


sealed trait ControllerServerId extends EngineServerId:
  def nodeId: NodeId

object ControllerServerId:

  case object ControllerPrimary extends ControllerServerId:
    val nodeId = NodeId.primary

  case object ControllerBackup extends ControllerServerId:
    val nodeId = NodeId.backup

  @javaApi
  val controllerPrimary: ControllerServerId = ControllerPrimary

  @javaApi
  val controllerBackup: ControllerServerId = ControllerBackup


final case class SubagentServerId(subagentId: SubagentId) extends EngineServerId

object SubagentServerId:
  @javaApi
  def of(subagentId: SubagentId): SubagentServerId =
    SubagentServerId(subagentId)
