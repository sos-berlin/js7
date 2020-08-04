package js7.proxy.javaapi.data

import js7.base.annotation.javaApi
import js7.data.item.{ItemId, TypedPath, VersionId}

@javaApi
trait JItemId[P <: TypedPath]
{
  final type Path = P
  protected type ScalaPath <: TypedPath
  protected type Underlying <: ItemId[ScalaPath]
  protected type UnderlyingPath <: ItemId[ScalaPath]

  def underlying: Underlying

  def path: P

  def versionId: VersionId =
    underlying.versionId
}
