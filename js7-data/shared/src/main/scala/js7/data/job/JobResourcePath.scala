package js7.data.job

import js7.base.annotation.javaApi
import js7.data.item.{InventoryItemPath, SignableSimpleItemPath}

final case class JobResourcePath(string: String)
extends SignableSimpleItemPath, InventoryItemPath.AttachableToAgent:

  def companion: SignableSimpleItemPath.Companion[? <: SignableSimpleItemPath] =
    JobResourcePath


object JobResourcePath extends SignableSimpleItemPath.Companion[JobResourcePath]:

  protected def unchecked(string: String) =
    new JobResourcePath(string)

  @javaApi
  def of(validName: String): JobResourcePath =
    mayThrow(validName)
