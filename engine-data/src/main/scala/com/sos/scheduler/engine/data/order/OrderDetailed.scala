package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits.InstantJsonFormat
import com.sos.scheduler.engine.base.sprayjson.SprayJson.JsonFormats.PathJsonFormat
import com.sos.scheduler.engine.data.filebased.FileBasedDetailed
import com.sos.scheduler.engine.data.jobchain.NodeId
import java.nio.file.Path
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderDetailed(
  overview: OrderOverview,
  file: Option[Path] = None,
  fileModifiedAt: Option[Instant] = None,
  sourceXml: Option[String] = None,
  priority: Int,
  initialNodeId: Option[NodeId] = None,
  endNodeId: Option[NodeId] = None,
  title: String,
  variables: Map[String, String] = Map())
extends OrderView with FileBasedDetailed {
  def orderKey = overview.orderKey

  private[engine] def occupyingClusterMemberId = overview.occupyingClusterMemberId

  private[engine] def processingState = overview.processingState

  private[engine] def nodeKey = overview.nodeKey
}

object OrderDetailed extends OrderView.Companion[OrderDetailed] {
  implicit val jsonFormat = jsonFormat9(apply)
}
