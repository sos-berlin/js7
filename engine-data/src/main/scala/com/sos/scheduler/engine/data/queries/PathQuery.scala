package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.filebased.{TypedPath, UnknownTypedPath}
import com.sos.scheduler.engine.data.folder.FolderPath

/**
  * @author Joacim Zschimmer
  */
sealed trait PathQuery {

  def patternString: String

  def isRecursive: Boolean

  /** PathQuery may apply to any `TypedPath`. */
  def matches[P <: TypedPath: TypedPath.Companion](path: P): Boolean

  def matchesAll = false

  def folderPath: FolderPath

  def toUriPath: String = patternString
}

object PathQuery {
  def apply[P <: TypedPath: TypedPath.Companion](pattern: String): PathQuery =
    if (pattern endsWith "/*")
      FolderOnly(FolderPath.fromTrailingSlash(pattern stripSuffix "*"))  // P is ignored. It's a FolderPath denoting a subtree of P
    else
    if (pattern endsWith "/")
      FolderTree(FolderPath.fromTrailingSlash(pattern))  // P is ignored. It's a FolderPath denoting a subtree of P
    else {
      val checkedTypedPath = implicitly[TypedPath.Companion[P]].apply(pattern)
      SinglePath(checkedTypedPath.string)
    }

  def apply(path: FolderPath, isRecursive: Boolean = true) = Folder(path, isRecursive)

  def apply(path: TypedPath): PathQuery =
    path match {
      case o: FolderPath ⇒ FolderTree(o)
      case o: TypedPath ⇒ SinglePath(o.string)
    }

  def fromUriPath[A <: TypedPath: TypedPath.Companion](path: String): PathQuery = apply[A](path)

  val All = FolderTree(FolderPath.Root)

  sealed trait Folder extends PathQuery

  object Folder {
    def apply(folder: FolderPath, isRecursive: Boolean) = if (isRecursive) FolderTree(folder) else FolderOnly(folder)
    def unapply(o: Folder): Option[(FolderPath, Boolean)] = Some((o.folderPath, o.isRecursive))
  }

  final case class FolderTree(folderPath: FolderPath) extends Folder {
    def patternString = folderPath.withTrailingSlash
    def isRecursive = true
    override val matchesAll = folderPath == FolderPath.Root
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = matchesAll || (folderPath isParentOf path)
  }

  final case class FolderOnly(folderPath: FolderPath) extends Folder {
    def patternString = folderPath.withTrailingSlash + "*"
    def isRecursive = false
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = folderPath == path.parent
  }

  final case class SinglePath(pathString: String) extends PathQuery {
    def patternString = pathString
    def isRecursive = false
    val folderPath = UnknownTypedPath(pathString).parent
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = path == as[P]
    def as[P <: TypedPath: TypedPath.Companion]: P = implicitly[TypedPath.Companion[P]].apply(pathString)
  }
}
