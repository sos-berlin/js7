package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.filebased.TypedPath
import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath

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
}

object PathQuery {
  def apply[P <: TypedPath: TypedPath.Companion](pattern: String): PathQuery =
    if (pattern.endsWith("/*"))  // P is ignored
      FolderOnly(pattern match {
        case "/*" ⇒ FolderPath.Root  // Root is denoted as FolderPath("/"), not FolderPath("") (which is rejected).
        case _ ⇒ FolderPath(pattern stripSuffix "/*")
      })
    else
      apply(toTypedPath(pattern))

  def apply(path: FolderPath, isRecursive: Boolean = true) = Folder(path, isRecursive)

  def apply(path: TypedPath): PathQuery =
    path match {
      case FolderPath.Root ⇒ All
      case o: FolderPath ⇒ FolderTree(o)
      case o: TypedPath ⇒ SinglePath(o.string)
    }

  def fromUriPath[A <: TypedPath: TypedPath.Companion](path: String): PathQuery = apply[A](path)

  private def toTypedPath[P <: TypedPath: TypedPath.Companion](path: String): TypedPath =
    if (path == "/")
      FolderPath(path)
    else if (path endsWith "/")
      FolderPath(path stripSuffix "/")
    else
      implicitly[TypedPath.Companion[P]].apply(path)

  case object All extends PathQuery {
    def patternString = "/"
    def isRecursive = true
    def folderPath = FolderPath.Root
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = true
    override def matchesAll = true
  }

  sealed trait Folder extends PathQuery

  object Folder {
    def apply(folder: FolderPath, isRecursive: Boolean) = if (isRecursive) FolderTree(folder) else FolderOnly(folder)
    def unapply(o: Folder): Option[(FolderPath, Boolean)] = Some((o.folderPath, o.isRecursive))
  }

  final case class FolderTree(folderPath: FolderPath) extends Folder {
    def patternString = folderPath.withTrailingSlash
    def isRecursive = true
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = folderPath isParentOf path
  }

  final case class FolderOnly(folderPath: FolderPath) extends Folder {
    def patternString = folderPath.withTrailingSlash + "*"
    def isRecursive = false
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = folderPath == path.parent
  }

  final case class SinglePath(pathString: String) extends PathQuery {
    def patternString = pathString
    def isRecursive = false
    val folderPath = JobPath(pathString).parent  // JobPath or any other type
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = path == as[P]
    def as[P <: TypedPath: TypedPath.Companion]: P = implicitly[TypedPath.Companion[P]].apply(pathString)
  }
}
