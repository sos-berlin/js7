package js7.data.agent

import js7.base.annotation.javaApi
import js7.data.item.UnsignedSimpleItemPath

/**
  * @author Joacim Zschimmer
  */
final case class AgentPath private(string: String)
extends DelegateId with UnsignedSimpleItemPath
{
  protected type Self = AgentPath

  val companion = AgentPath

  override def toString = s"Agent:$string"  // instead of AgentRef:
}

object AgentPath
extends DelegateId.Companion[AgentPath]
with UnsignedSimpleItemPath.Companion[AgentPath]
{
  /** Internal use only. */
  private[js7] val empty = new AgentPath("")

  override val itemTypeName = "AgentRef"  // May deadlock: AgentRef.typeName
  override val pathTypeName = "Agent"

  protected def unchecked(string: String) = new AgentPath(string)

  @javaApi
  def of(validName: String) = apply(validName)
}
