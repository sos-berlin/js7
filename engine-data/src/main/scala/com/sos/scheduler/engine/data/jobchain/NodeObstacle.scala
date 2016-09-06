package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import java.time.Duration
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait NodeObstacle

object NodeObstacle {

  /**
    * &lt;job_chain_node action="stop">.
    */
  object Stopping
  extends NodeObstacle

  /**
    * &lt;job_chain_node delay="...">.
    */
  final case class Delaying(duration: Duration)
  extends NodeObstacle

  implicit val NodeObstacleJsonFormat = TypedJsonFormat[NodeObstacle](
    Subtype(jsonFormat0(() ⇒ Stopping)),
    Subtype(jsonFormat1(Delaying)))
}
