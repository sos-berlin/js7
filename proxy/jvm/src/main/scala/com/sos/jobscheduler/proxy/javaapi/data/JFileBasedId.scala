package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.data.filebased.{FileBasedId, TypedPath, VersionId}

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
