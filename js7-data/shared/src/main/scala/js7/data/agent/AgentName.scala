package js7.data.agent

import js7.base.annotation.javaApi
import js7.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class AgentName private(string: String) extends GenericString
{
  def companion = AgentName
}

object AgentName extends GenericString.NameValidating[AgentName]
{
  protected def unchecked(string: String) = new AgentName(string)

  @javaApi
  def of(validName: String) = apply(validName)
}
