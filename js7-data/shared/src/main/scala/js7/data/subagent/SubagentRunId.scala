package js7.data.subagent

import js7.base.generic.GenericString
import js7.base.utils.Base64UUID

final case class SubagentRunId(base64UUID: Base64UUID) extends GenericString
{
  def string = base64UUID.string

  override def toString = typedToString
}

object SubagentRunId extends GenericString.NonEmpty[SubagentRunId]
{
  val empty = SubagentRunId(Base64UUID.zero)

  protected def unchecked(string: String) =
    throw new NotImplementedError

  override def checked(string: String) =
    Base64UUID.checked(string) map apply
}
