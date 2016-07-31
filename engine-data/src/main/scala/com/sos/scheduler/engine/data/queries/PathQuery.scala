package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.generic.IsString
import com.sos.scheduler.engine.data.filebased.{AbsolutePath, TypedPath}
import com.sos.scheduler.engine.data.folder.FolderPath

/**
  * @author Joacim Zschimmer
  */
final case class PathQuery(string: String) extends IsString {
  require(string.nonEmpty, "PathQuery must not be empty")
  require((string startsWith "/") && !(string contains "/.."), s"Invalid PathQuery: $string")

  val matchesAll = string == "/"

  def matches(path: TypedPath): Boolean =
    matchesAll || (
      if (string endsWith "/") path.string startsWith string
      else path.string == string)

  def folderPath: FolderPath = reduceToAbsolutePath match {
    case o: FolderPath ⇒ o
    case o: AbsolutePath ⇒ o.parent
    case _ ⇒ FolderPath.Root
  }

  private[engine] def reduce[A <: TypedPath: TypedPath.Companion]: IsString =
    reduceToAbsolutePath match {
      case path: AbsolutePath.Untyped ⇒ implicitly[TypedPath.Companion[A]].apply(path.string)
      case o ⇒ o
    }

  private def reduceToAbsolutePath =
    if (matchesAll) PathQuery.All
    else if (string endsWith "/") FolderPath(string stripSuffix "/")
    else AbsolutePath.Untyped(string)
}

object PathQuery extends IsString.Companion[PathQuery] {
  val All = PathQuery("/")

  def apply(folderPath: FolderPath) = new PathQuery((folderPath.string stripSuffix "/") + "/")

  def apply(path: TypedPath) = new PathQuery(path.string)
}
