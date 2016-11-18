package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class FolderPath(string: String) extends TypedPath {

  validate()

  def companion = FolderPath

  def subfolder(name: String): FolderPath = {
    require(!name.contains('/'), "Name must not contain a slash '/'")
    FolderPath(s"${string stripSuffix "/"}/$name")
  }

  def isParentOf(path: TypedPath): Boolean =
    this == path.parent

  def isAncestorOf(path: TypedPath): Boolean =
    (path.string startsWith withTrailingSlash) ||
      PartialFunction.cond(path) {
        case path: FolderPath ⇒ this == path
      }
}

object FolderPath extends TypedPath.Companion[FolderPath] {

  val Root = FolderPath("/")

  def fromTrailingSlash(string: String) = {
    require(string endsWith "/", "Trailing slash required for FolderPath")
    FolderPath(if (string == "/") string else string stripSuffix "/")
  }

  // 'def' due to mutual singleton dependency of this and FileBasedType
  def fileBasedType = FileBasedType.Folder

  override protected[engine] def isSingleSlashAllowed = true
  override protected[engine] def isCommaAllowed = false
}
