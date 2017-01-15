package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.filebased.TypedPath

final case class FolderPath(string: String) extends TypedPath {

  validate()

  def companion = FolderPath

  def subfolder(name: String): FolderPath = {
    require(!name.contains('/'), "Name must not contain a slash '/'")
    FolderPath(s"${string stripSuffix "/"}/$name")
  }

  def isParentOf(path: TypedPath): Boolean =
    path != FolderPath.Root && this == path.parent

  def isAncestorOf(path: TypedPath): Boolean =
    (path.string startsWith withTrailingSlash) ||
      PartialFunction.cond(path) {
        case path: FolderPath ⇒ this == path
      }
}

object FolderPath extends TypedPath.Companion[FolderPath] {

  val Root = FolderPath("/")
  override lazy val filenameExtension = "/"

  def fromTrailingSlash(string: String) = {
    require(string endsWith "/", "Trailing slash required for FolderPath")
    FolderPath(if (string == "/") string else string stripSuffix "/")
  }

  override protected[engine] def isSingleSlashAllowed = true
  override protected[engine] def isCommaAllowed = false
}
