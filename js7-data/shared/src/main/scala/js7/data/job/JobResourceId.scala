package js7.data.job

import js7.base.annotation.javaApi
import js7.data.item.SignableSimpleItemId

final case class JobResourceId(string: String) extends SignableSimpleItemId
{
  def companion = JobResourceId
}

object JobResourceId extends SignableSimpleItemId.Companion[JobResourceId]
{
  val itemTypeName = JobResource.typeName

  protected def unchecked(string: String) =
    new JobResourceId(string)

  @javaApi
  def of(validName: String): JobResourceId =
    apply(validName)
}
