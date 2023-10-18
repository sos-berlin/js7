package js7.data.job

import js7.base.annotation.javaApi
import js7.data.item.{InventoryItemPath, SignableSimpleItemPath}

final case class JobResourcePath(string: String)
extends SignableSimpleItemPath
with InventoryItemPath.AttachableToAgent:

  def companion = JobResourcePath


object JobResourcePath extends SignableSimpleItemPath.Companion[JobResourcePath]:
  // May deadlock: override val itemTypeName = JobResource.typeName

  protected def unchecked(string: String) =
    new JobResourcePath(string)

  @javaApi
  def of(validName: String): JobResourcePath =
    apply(validName)
