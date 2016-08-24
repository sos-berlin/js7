package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.lazyRootFormat
import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.filebased.{FileBasedOverview, FileBasedState}
import com.sos.scheduler.engine.data.jobchain.{NodeId, NodeKey}
import com.sos.scheduler.engine.data.order.OrderOverview._
import com.sos.scheduler.engine.data.queries.QueryableOrder
import com.sos.scheduler.engine.data.scheduler.ClusterMemberId
import java.time.Instant
import scala.collection.immutable
import scala.language.implicitConversions
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

/**
  * @author Joacim Zschimmer
  */
final case class OrderOverview(
  path: OrderKey,
  fileBasedState: FileBasedState,
  sourceType: OrderSourceType,
  nodeId: NodeId,
  processingState: OrderProcessingState,
  obstacles: Set[OrderObstacle] = Set(),
  nextStepAt: Option[Instant] = None,
  occupyingClusterMemberId: Option[ClusterMemberId] = None,
  liveChanged: Option[LiveChanged] = None)
extends FileBasedOverview with QueryableOrder {

  def orderKey: OrderKey = path

  def nodeKey: NodeKey = NodeKey(orderKey.jobChainPath, nodeId)

  def isSetback = processingState.isInstanceOf[OrderProcessingState.Setback]

  def isBlacklisted = processingState == OrderProcessingState.Blacklisted

  def isSuspended = obstacles contains OrderObstacle.Suspended
}

object OrderOverview {
  sealed trait LiveChanged
  final case class Replaced(overview: OrderOverview) extends LiveChanged
  case object Removed extends LiveChanged

  private implicit val ConfigurationJsonType = TypedJsonFormat.asLazy(
    TypedJsonFormat[LiveChanged](
    Subtype(jsonFormat1(Replaced)),
    Subtype(jsonFormat0(() ⇒ Removed))))
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat

  implicit val MyJsonFormat: RootJsonFormat[OrderOverview] = lazyRootFormat(jsonFormat9(apply))

  implicit val ordering: Ordering[OrderOverview] = Ordering by { o ⇒ (o.orderKey.jobChainPath, o.nodeId, o.orderKey.id) }

  final class Statistics(val orderOverviews: immutable.Seq[OrderOverview]) {
    def count = orderOverviews.size
    lazy val inProcessCount = orderOverviews count { _.processingState.isInstanceOf[OrderProcessingState.InTaskProcess] }
    lazy val suspendedCount = orderOverviews count { _.isSuspended }
    lazy val blacklistedCount = orderOverviews count { _.isBlacklisted }
  }
}
