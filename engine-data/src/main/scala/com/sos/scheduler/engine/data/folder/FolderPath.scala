package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.filebased.{AbsolutePath, TypedPath}

final case class FolderPath(string: String) extends TypedPath {
  import FolderPath._
  validate()

  def companion = FolderPath

  def subfolder(name: String): FolderPath = {
    require(!name.contains('/'), "Name must not contain a slash '/'")
    FolderPath(s"${string stripSuffix "/"}/$name")
  }

  def resolve[P <: TypedPath: TypedPath.Companion](path: String) =
    implicitly[TypedPath.Companion[P]].apply(absoluteString(this, path))

  def isParentOf(path: TypedPath): Boolean =
    path != FolderPath.Root && this == parentOf(path)

  def isAncestorOf(path: TypedPath): Boolean =
    (path.string startsWith withTrailingSlash) ||
      PartialFunction.cond(path) {
        case path: FolderPath ⇒ this == path
      }
}

object FolderPath extends TypedPath.Companion[FolderPath] {

  val Root = FolderPath("/")
  override lazy val filenameExtension = "/"

  override protected[engine] def isSingleSlashAllowed = true
  override protected[engine] def isCommaAllowed = false

  def fromTrailingSlash(string: String) = {
    require(string endsWith "/", "Trailing slash required for FolderPath")
    FolderPath(if (string == "/") string else string stripSuffix "/")
  }

  def parentOf(path: TypedPath): FolderPath =
    path.string lastIndexOf '/' match {
      case 0 if path.string == "/" ⇒ throw new IllegalStateException("Root path has no parent folder")
      case 0 ⇒ FolderPath.Root
      case -1 ⇒ FolderPath.Root // In case of ProcessClass.Default (the empty string)
      case n ⇒ FolderPath(path.string.substring(0, n))
    }

  /**
   * An absolute `path` starting with "/" is used as given.
   * A relative `path` not starting with "/" is used relative to `defaultFolder`.
   */
  private def absoluteString(defaultFolder: FolderPath, path: String): String = {
    if (AbsolutePath.isAbsolute(path))
      path
    else
      s"${defaultFolder.withTrailingSlash}${path stripPrefix "./"}"
  }
}
