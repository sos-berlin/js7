package com.sos.jobscheduler.data.filebased

import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class BFileBased(id: FileBasedId[BPath], content: String) extends FileBased {
  type Self = BFileBased

  val companion = BFileBased

  def withId(id: FileBasedId[BPath]) = copy(id = id)
}

object BFileBased extends FileBased.Companion[BFileBased] {
  type ThisFileBased = BFileBased
  type Path = BPath

  def typedPathCompanion = BPath
}

case class BPath(string: String) extends TypedPath {
  def companion = BPath
}

object BPath extends TypedPath.Companion[BPath] {
  val sourceTypeToFilenameExtension: Map[SourceType, String] = Map(
    SourceType.Json → ".b.json")
}
