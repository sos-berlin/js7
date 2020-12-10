package js7.data.agent

import js7.base.annotation.javaApi
import js7.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class AgentId private(string: String) extends GenericString
{
  def companion = AgentId
}

object AgentId extends GenericString.NameValidating[AgentId]
{
  protected def unchecked(string: String) = new AgentId(string)

  @javaApi
  def of(validName: String) = apply(validName)
}
