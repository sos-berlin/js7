package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat
import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat.Subtype
import com.sos.scheduler.engine.data.common.Obstacle
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait FileBasedObstacle
extends Obstacle

object FileBasedObstacle {

  case object Missing
  extends FileBasedObstacle

  final case class BadState(state: FileBasedState, message: Option[String])
  extends FileBasedObstacle {
    override def toString = (List(state) ++ message).mkString("BadState(", " ", ")")
  }

  //case object ToBeChanged ?
  //case object ToBeRemoved ?

  implicit private val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val FileBasedJsonFormat = TypedJsonFormat[FileBasedObstacle](
    Subtype(jsonFormat0(() ⇒ Missing)),
    Subtype(jsonFormat2(BadState)))
}
