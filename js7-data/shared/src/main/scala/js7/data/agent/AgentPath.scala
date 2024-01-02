package js7.data.agent

import js7.base.annotation.javaApi
import js7.data.cluster.ClusterId
import js7.data.delegate.DelegateId
import js7.data.item.UnsignedSimpleItemPath

/**
  * @author Joacim Zschimmer
  */
final case class AgentPath private(string: String)
extends UnsignedSimpleItemPath, DelegateId, ClusterId:

  protected type Self = AgentPath

  val companion: AgentPath.type = AgentPath

  override def toString = s"Agent:$string"  // instead of AgentRef:


object AgentPath
extends DelegateId.Companion[AgentPath], UnsignedSimpleItemPath.Companion[AgentPath]:
  type Item = AgentRef

  /** Internal use only. */
  private[js7] val empty = new AgentPath("")

  override val itemTypeName = "AgentRef"  // May deadlock: AgentRef.typeName
  override val pathTypeName = "Agent"

  protected def unchecked(string: String) = new AgentPath(string)

  @javaApi
  def of(validName: String) = apply(validName)
