package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class FolderPath(string: String) extends TypedPath {
  validate()

  def fileBasedType = FileBasedType.folder

  def subfolder(name: String): FolderPath = {
    require(!name.contains('/'), "Name must not contain a slash '/'")
    FolderPath(s"${string stripSuffix "/"}/$name")
  }
}

object FolderPath extends TypedPath.Companion[FolderPath] {
  val Root = FolderPath("/")
}
