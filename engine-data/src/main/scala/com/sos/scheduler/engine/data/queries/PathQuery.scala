package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.filebased.TypedPath
import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.order.OrderKey

/**
  * @author Joacim Zschimmer
  */
sealed trait PathQuery {

  def patternString: String

  /** Not type-safe - the type is ignored. */
  def matches[P <: TypedPath: TypedPath.Companion](path: P): Boolean

  def matchesAll = false

  def folderPath: FolderPath
}

object PathQuery {
//  import spray.json._
//  implicit val MyJsonFormat = new JsonFormat[PathQuery] {
//    def read(json: JsValue) = PathQuery(json.asInstanceOf[JsString].value)
//    def write(o: PathQuery) = JsString(o.patternString)
//  }

  def apply[A <: TypedPath: TypedPath.Companion](pattern: String): PathQuery =
    apply(toTypedPath(pattern))

  def apply(path: FolderPath) = Folder(path)

  def apply(path: TypedPath): PathQuery =
    path match {
      case FolderPath.Root ⇒ All
      case o: FolderPath ⇒ Folder(o)
      case o: TypedPath ⇒ SinglePath(o.string)
    }

  private def toTypedPath[A <: TypedPath: TypedPath.Companion](path: String): TypedPath =
    if (path == "/")
      FolderPath(path)
    else if (path endsWith "/")
      FolderPath(path stripSuffix "/")
    else
      implicitly[TypedPath.Companion[A]].apply(path)

  case object All extends PathQuery {
    def patternString = "/"
    def folderPath = FolderPath.Root
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = true
    override def matchesAll = true
  }

  final case class Folder(folderPath: FolderPath) extends PathQuery {
    def patternString = folderPath.withTrailingSlash
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = folderPath isParentOf path
  }

  final case class SinglePath(pathString: String) extends PathQuery {
    def patternString = pathString
    val folderPath = JobPath(pathString).parent  // JobPath or any other type
    def matches[P <: TypedPath: TypedPath.Companion](path: P) = path == as[P]
    def as[P <: TypedPath: TypedPath.Companion]: P = implicitly[TypedPath.Companion[P]].apply(pathString)
  }
}
