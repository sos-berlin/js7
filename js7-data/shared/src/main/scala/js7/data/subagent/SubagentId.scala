package js7.data.subagent

import js7.base.annotation.javaApi
import js7.base.auth.UserId
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.delegate.DelegateId
import js7.data.item.InventoryItemPath.AssignableToAgent
import js7.data.item.UnsignedSimpleItemPath

final case class SubagentId(string: String)
extends UnsignedSimpleItemPath with DelegateId with AssignableToAgent
{
  def companion = SubagentId

  def toUserId = UserId(string)
}

object SubagentId
extends DelegateId.Companion[SubagentId]
with UnsignedSimpleItemPath.Companion[SubagentId]
{
  override val itemTypeName = "Subagent"
  override val pathTypeName = itemTypeName

  protected def unchecked(string: String) =
    new SubagentId(string)

  override def checked(string: String): Checked[SubagentId] =
    UserId.checked(string)
      .flatMap(_ => super.checked(string))

  @javaApi
  def of(string: String): SubagentId =
    checked(string).orThrow
}
