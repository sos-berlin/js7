package js7.data.subagent

import js7.base.annotation.javaApi
import js7.base.auth.UserId
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.delegate.DelegateId
import js7.data.item.InventoryItemPath.AttachableToAgent
import js7.data.item.UnsignedSimpleItemPath
import js7.data.node.NodeName

final case class SubagentId(string: String)
extends UnsignedSimpleItemPath, DelegateId, AttachableToAgent:

  def companion: UnsignedSimpleItemPath.Companion[? <: UnsignedSimpleItemPath] = SubagentId

  def toUserId: Checked[UserId] =
    UserId.checked(string)

  def toNodeName: Checked[NodeName] =
    NodeName.checked(string)


object SubagentId
extends DelegateId.Companion[SubagentId], UnsignedSimpleItemPath.Companion[SubagentId]:

  override val itemTypeNameAliases = Seq("SubagentRef")

  protected def unchecked(string: String) =
    new SubagentId(string)

  override def checked(string: String): Checked[SubagentId] =
    UserId.checked(string)
      .flatMap(_ => super.checked(string))

  @javaApi
  def of(string: String): SubagentId =
    checked(string).orThrow

  def legacyLocalFromAgentPath(agentPath: AgentPath): SubagentId =
    SubagentId(agentPath.string + "-1")
