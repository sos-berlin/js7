package com.sos.jobscheduler.data.filebased

import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class TestFileBased(path: TestPath, content: String) extends FileBased {
  type Self = TestFileBased

  def companion = TestFileBased
}

object TestFileBased extends FileBased.Companion[TestFileBased] {
  type ThisTypedPath = TestPath

  def typedPathCompanion = TestPath
}

case class TestPath(string: String) extends TypedPath {
  def companion = TestPath
}

object TestPath extends TypedPath.Companion[TestPath] {
  def sourceTypeToFilenameExtension = Map.empty
}
