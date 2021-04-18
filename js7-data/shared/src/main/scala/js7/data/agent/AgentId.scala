package js7.data.agent

import js7.base.annotation.javaApi
import js7.data.item.UnsignedSimpleItemId

/**
  * @author Joacim Zschimmer
  */
final case class AgentId private(string: String) extends UnsignedSimpleItemId
{
  protected type Self = AgentId

  val companion = AgentId
}

object AgentId extends UnsignedSimpleItemId.Companion[AgentId]
{
  def itemTypeName = AgentRef.typeName

  protected def unchecked(string: String) = new AgentId(string)

  @javaApi
  def of(validName: String) = apply(validName)
}
