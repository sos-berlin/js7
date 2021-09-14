package js7.data.agent

import js7.base.annotation.javaApi
import js7.data.item.UnsignedSimpleItemPath

/**
  * @author Joacim Zschimmer
  */
final case class AgentPath private(string: String) extends UnsignedSimpleItemPath
{
  protected type Self = AgentPath

  val companion = AgentPath

  override def toString = s"Agent:$string"  // instead of AgentRef:
}

object AgentPath extends UnsignedSimpleItemPath.Companion[AgentPath]
{
  /** Internal use only. */
  private[js7] val empty = new AgentPath("")

  def itemTypeName = AgentRef.typeName

  protected def unchecked(string: String) = new AgentPath(string)

  @javaApi
  def of(validName: String) = apply(validName)
}
