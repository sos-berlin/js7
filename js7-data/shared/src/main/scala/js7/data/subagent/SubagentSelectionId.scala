package js7.data.subagent

import js7.base.annotation.javaApi
import js7.base.auth.UserId
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.delegate.DelegateId
import js7.data.item.{InventoryItemPath, UnsignedSimpleItemPath}

final case class SubagentSelectionId(string: String)
extends UnsignedSimpleItemPath, DelegateId, InventoryItemPath.AttachableToAgent:

  def companion = SubagentSelectionId

  def toUserId = UserId(string)

  // A SubagentId may be used as a SubagentSelectionId
  def toSubagentId = SubagentId(string)


object SubagentSelectionId
extends DelegateId.Companion[SubagentSelectionId],
  UnsignedSimpleItemPath.Companion[SubagentSelectionId]:

  def fromSubagentId(subagentId: SubagentId): SubagentSelectionId =
    SubagentSelectionId(subagentId.string)

  type Item = SubagentSelection

  override val itemTypeName = "SubagentSelection"
  override val pathTypeName = itemTypeName

  protected def unchecked(string: String) =
    new SubagentSelectionId(string)

  @javaApi
  def of(string: String): SubagentSelectionId =
    checked(string).orThrow
