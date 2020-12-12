package js7.data.agent

import js7.base.annotation.javaApi
import js7.data.item.SimpleItemId

/**
  * @author Joacim Zschimmer
  */
final case class AgentId private(string: String) extends SimpleItemId
{
  protected type Self = AgentId

  def companion = AgentId

  override def toString = s"Agent:$string"
}

object AgentId extends SimpleItemId.Companion[AgentId]
{
  protected def unchecked(string: String) = new AgentId(string)

  @javaApi
  def of(validName: String) = apply(validName)
}
