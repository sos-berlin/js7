package js7.proxy.javaapi.data

import js7.base.annotation.javaApi
import js7.data.filebased.{FileBasedId, TypedPath, VersionId}

@javaApi
trait JFileBasedId[P <: TypedPath]
{
  final type Path = P
  protected type ScalaPath <: TypedPath
  protected type Underlying <: FileBasedId[ScalaPath]
  protected type UnderlyingPath <: FileBasedId[ScalaPath]

  def underlying: Underlying

  def path: P

  def versionId: VersionId =
    underlying.versionId
}
