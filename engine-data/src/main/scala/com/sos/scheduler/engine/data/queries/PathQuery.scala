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

  def apply(pattern: String): PathQuery =
    if (pattern == "/")
      All
    else if (pattern endsWith "/")
      Folder(FolderPath(pattern stripSuffix "/"))
    else
      SinglePath(pattern)

  def apply(path: FolderPath) = Folder(path)

  def apply(path: TypedPath): SinglePath =
    path match {
      case orderKey: OrderKey ⇒ apply(orderKey)
      case _ ⇒ new SinglePath(path.string)
    }

  /**
    * Comma in path leads to Exception when tried as other TypedPath.
    */
  def apply(path: OrderKey): Nothing =
    throw new IllegalArgumentException("OrderKey is not applicable for PathQuery")

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
