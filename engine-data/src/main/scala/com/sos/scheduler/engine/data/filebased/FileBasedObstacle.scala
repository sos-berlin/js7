package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait FileBasedObstacle

object FileBasedObstacle {

  case object Missing
  extends FileBasedObstacle

  final case class BadState(state: FileBasedState, message: Option[String] = None)
  extends FileBasedObstacle {
    override def toString = (List(state) ++ message).mkString("BadState(", " ", ")")
  }

  sealed trait LiveChanged
  extends FileBasedObstacle

  case object Replaced
  extends LiveChanged

  case object Removed
  extends LiveChanged

  implicit val LiveChangedJsonFormat = TypedJsonFormat[LiveChanged](
    Subtype(jsonFormat0(() ⇒ Replaced)),
    Subtype(jsonFormat0(() ⇒ Removed)))

  implicit private val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val FileBasedJsonFormat = TypedJsonFormat[FileBasedObstacle](
    Subtype(jsonFormat0(() ⇒ Missing)),
    Subtype(jsonFormat2(BadState)),
    Subtype[LiveChanged])
}
