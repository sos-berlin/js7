package js7.proxy.javaapi.data.item

import js7.base.annotation.javaApi
import js7.data.item.{ItemId, TypedPath, VersionId}

@javaApi
trait JItemId[P <: TypedPath]
{
  final type Path = P
  protected type ScalaPath <: TypedPath
  protected type AsScala <: ItemId[ScalaPath]

  def asScala: AsScala

  def path: P

  def versionId: VersionId =
    asScala.versionId
}
