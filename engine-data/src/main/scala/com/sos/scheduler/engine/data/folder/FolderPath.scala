package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class FolderPath(string: String) extends TypedPath {

  if (string != "/") {
    validate()
  }

  def companion = FolderPath

  def subfolder(name: String): FolderPath = {
    require(!name.contains('/'), "Name must not contain a slash '/'")
    FolderPath(s"${string stripSuffix "/"}/$name")
  }

  def isParentOf(path: TypedPath): Boolean = path.string startsWith withTrailingSlash
}

object FolderPath extends TypedPath.Companion[FolderPath] {

  val Root = FolderPath("/")

  // 'def' due to mutual singleton dependency of this and FileBasedType
  def fileBasedType = FileBasedType.folder
}
