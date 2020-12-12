package js7.proxy.javaapi.data.item

import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.data.item.{ItemPath, VersionId, VersionedItemId}

@javaApi
trait JItemId[P <: ItemPath]
{
  final type Path = P
  protected type ScalaPath <: ItemPath
  protected type AsScala <: VersionedItemId[ScalaPath]

  def asScala: AsScala

  def path: P

  @Nonnull
  def versionId: VersionId =
    asScala.versionId
}
