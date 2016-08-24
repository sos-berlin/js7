package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.filebased.FileBasedDetails
import java.nio.file.Path
import java.time.Instant
import spray.json.DefaultJsonProtocol._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits.PathJsonFormat
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits.InstantJsonFormat

/**
  * @author Joacim Zschimmer
  */
final case class OrderDetails(
  overview: OrderOverview,
  file: Option[Path],
  fileModifiedAt: Option[Instant],
  sourceXml: Option[String],
  variables: Map[String, String])
extends FileBasedDetails {
  def orderKey = overview.orderKey
}

object OrderDetails {
  implicit val MyJsonFormat = jsonFormat5(apply)
}
