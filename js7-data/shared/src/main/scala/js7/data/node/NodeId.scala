package js7.data.node

import js7.base.auth.UserId
import js7.base.generic.GenericString
import js7.base.problem.Checked

final case class NodeId private(string: String)
extends GenericString

object NodeId extends GenericString.NonEmpty[NodeId]
{
  def unchecked(string: String): NodeId =
    new NodeId(string)

  override def checked(o: String): Checked[NodeId] =
    UserId.checked(o)
      .flatMap(u => super.checked(u.string))
}
