package js7.data.subagent

import js7.base.annotation.javaApi
import js7.base.auth.UserId
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.delegate.DelegateId
import js7.data.item.{InventoryItemPath, UnsignedSimpleItemPath}

final case class SubagentBundleId(string: String)
extends UnsignedSimpleItemPath, DelegateId, InventoryItemPath.AttachableToAgent:

  def companion: SubagentBundleId.type = SubagentBundleId

  def toUserId: UserId =
    UserId(string)

  // A SubagentId may be used as a SubagentBundleId
  def toSubagentId: SubagentId = SubagentId(string)


object SubagentBundleId
extends DelegateId.Companion[SubagentBundleId],
  UnsignedSimpleItemPath.Companion[SubagentBundleId]:

  def fromSubagentId(subagentId: SubagentId): SubagentBundleId =
    SubagentBundleId(subagentId.string)

  type Item = SubagentBundle

  override val itemTypeNameAliases = Seq("SubagentSelection"/*COMPATIBLE with v2.7.1*/)

  protected def unchecked(string: String) =
    new SubagentBundleId(string)

  @javaApi
  def of(string: String): SubagentBundleId =
    checked(string).orThrow
