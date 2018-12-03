package com.sos.jobscheduler.data.filebased

import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AFileBased(id: FileBasedId[APath], content: String) extends FileBased {
  type Self = AFileBased

  val companion = AFileBased

  def withId(id: FileBasedId[APath]) = copy(id = id)
}

object AFileBased extends FileBased.Companion[AFileBased] {
  type ThisFileBased = AFileBased
  type Path = APath

  def typedPathCompanion = APath
}

case class APath(string: String) extends TypedPath
{
  def companion = APath
}

object APath extends TypedPath.Companion[APath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Json → ".a.json",
    SourceType.Txt → ".a.txt")

  def unchecked(string: String) = new APath(string)
}
