package  js7.data_for_java.item

import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.data.item.{ItemPath, VersionId, VersionedItemId}

@javaApi
trait JVersionedItemId[P <: ItemPath]
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
