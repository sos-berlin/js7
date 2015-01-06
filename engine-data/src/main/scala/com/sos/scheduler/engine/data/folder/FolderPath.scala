package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class FolderPath(string: String) extends TypedPath {
  requireIsAbsolute()

  def fileBasedType = FileBasedType.folder
}
