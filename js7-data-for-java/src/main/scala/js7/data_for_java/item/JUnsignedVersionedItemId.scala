package js7.data_for_java.item

import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.data.item.{UnsignedVersionedItemId, VersionId, VersionedControlPath}

@javaApi
trait JUnsignedVersionedItemId[P <: VersionedControlPath]
{
  final type Path = P
  protected type ScalaPath <: VersionedControlPath
  protected type AsScala <: UnsignedVersionedItemId[ScalaPath]

  def asScala: AsScala

  def path: P

  @Nonnull
  def versionId: VersionId =
    asScala.versionId
}
