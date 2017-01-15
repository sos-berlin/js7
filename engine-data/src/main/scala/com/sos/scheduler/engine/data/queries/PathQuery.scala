package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.serial.PathAndParameterSerializable
import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits.RichJsValue
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.data.filebased.{TypedPath, UnknownTypedPath}
import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.queries.PathQuery._
import scala.language.implicitConversions
import spray.json.{JsObject, JsString, JsValue, RootJsonFormat}

/**
  * @author Joacim Zschimmer
  */
sealed trait PathQuery {

  def patternString: String

  final def matchesAnyType(path: TypedPath): Boolean =
    path match {
      case o: FolderPath ⇒ matches(o)
      case o ⇒ matches(o.asTyped[UnknownTypedPath])
    }

  /** PathQuery may apply to any `TypedPath`. */
  def matches[P <: TypedPath: TypedPath.Companion](path: P): Boolean

  def matchesAll = false

  def withRecursive(recursive: Boolean): PathQuery =
    this match {
      case o: FolderTree if !recursive ⇒ FolderOnly(o.folderPath)
      case o: FolderOnly if recursive ⇒ FolderTree(o.folderPath)
      case o ⇒ o
    }

  def typedPath[P <: TypedPath: TypedPath.Companion]: TypedPath

  def folderPath: FolderPath

  def toUriPath: String = patternString

  def toPathAndParameters[P <: TypedPath: TypedPath.Companion]: (String, Map[String, String]) =
    PathQuery.pathAndParameterSerializable.toPathAndParameters(this)
}

object PathQuery {
  val All = FolderTree(FolderPath.Root)
  val PathName = "path"

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

  implicit def apply(path: TypedPath): PathQuery =
    path match {
      case o: FolderPath ⇒ FolderTree(o)
      case o: TypedPath ⇒ SinglePath(o.string)
    }

  def fromUriPath[A <: TypedPath: TypedPath.Companion](path: String): PathQuery = apply[A](path)

  sealed trait Folder extends PathQuery {
    def isRecursive: Boolean
  }

  object Folder {
    def apply(folder: FolderPath, isRecursive: Boolean) = if (isRecursive) FolderTree(folder) else FolderOnly(folder)
    def unapply(o: Folder): Option[(FolderPath, Boolean)] = Some((o.folderPath, o.isRecursive))
  }

  final case class FolderTree(folderPath: FolderPath) extends Folder {
    def patternString = folderPath.withTrailingSlash
    def isRecursive = true
    override val matchesAll = folderPath == FolderPath.Root
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = matchesAll || (folderPath isAncestorOf path)
    def typedPath[Ignored <: TypedPath: TypedPath.Companion] = folderPath
  }

  final case class FolderOnly(folderPath: FolderPath) extends Folder {
    def patternString = folderPath.withTrailingSlash + "*"
    def isRecursive = false
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = folderPath isParentOf path
    def typedPath[Ignored <: TypedPath: TypedPath.Companion] = folderPath
  }

  final case class SinglePath(pathString: String) extends PathQuery {
    def patternString = pathString
    def isRecursive = false
    val folderPath = FolderPath.parentOf(UnknownTypedPath(pathString))
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = path == as[P]
    def typedPath[P <: TypedPath: TypedPath.Companion]: TypedPath = as[P]
    def as[P <: TypedPath: TypedPath.Companion]: P = implicitly[TypedPath.Companion[P]].apply(pathString)
  }

  def jsonFormat[P <: TypedPath: TypedPath.Companion]: RootJsonFormat[PathQuery] =
    new RootJsonFormat[PathQuery] {
      def write(q: PathQuery) = JsObject((q != All option (PathName → JsString(q.toUriPath))).toMap)

      def read(json: JsValue) =
        json.asJsObject.fields.get(PathName) match {
          case Some(path) ⇒ PathQuery[P](path.asJsString.value)
          case None ⇒ PathQuery.All
        }
  }

  final def pathAndParameterSerializable[P <: TypedPath: TypedPath.Companion] =
    new PathAndParameterSerializable[PathQuery] {
      def toPathAndParameters(q: PathQuery) = q.toUriPath → Map()

      def fromPathAndParameters(pathAndParameters: (String, Map[String, String])) = {
        val (path, _) = pathAndParameters
        PathQuery[P](path)
      }
    }
}
