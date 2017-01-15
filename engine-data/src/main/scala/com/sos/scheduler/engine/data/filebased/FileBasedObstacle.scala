package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.filebaseds.TypedPathRegister.TypedPathJsonFormat
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait FileBasedObstacle

object FileBasedObstacle {

  final case class BadState(state: FileBasedState, error: Option[String] = None)
  extends FileBasedObstacle {
    override def toString = (List(state) ++ error).mkString("BadState(", " ", ")")
  }

  sealed trait LiveChanged
  extends FileBasedObstacle

  final case class Replaced(error: Option[String] = None)
  extends LiveChanged

  case object Removed
  extends LiveChanged

  final case class MissingRequisites(paths: Set[TypedPath])
  extends FileBasedObstacle {
    override def toString = paths.mkString("MissingRequisites(", ", ", ")")
  }

  object MissingRequisites {
    implicit val MyJsonFormat = jsonFormat1(apply)
  }

  implicit val LiveChangedJsonFormat = TypedJsonFormat[LiveChanged](
    Subtype(jsonFormat1(Replaced)),
    Subtype(jsonFormat0(() ⇒ Removed)))

  implicit private val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val FileBasedJsonFormat = TypedJsonFormat[FileBasedObstacle](
    Subtype(jsonFormat2(BadState)),
    Subtype[LiveChanged],
    Subtype[MissingRequisites])
}
