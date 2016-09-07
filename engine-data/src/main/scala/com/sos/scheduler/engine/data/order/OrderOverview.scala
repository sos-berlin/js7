package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.lazyRootFormat
import com.sos.scheduler.engine.data.filebased.FileBasedState
import com.sos.scheduler.engine.data.jobchain.{NodeId, NodeKey}
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
  historyId: Option[OrderHistoryId] = None,
  obstacles: Set[OrderObstacle] = Set(),
  startedAt: Option[Instant] = None,
  nextStepAt: Option[Instant] = None,
  occupyingClusterMemberId: Option[ClusterMemberId] = None)
extends OrderView with QueryableOrder {

  def orderKey: OrderKey = path

  def nodeKey: NodeKey = NodeKey(orderKey.jobChainPath, nodeId)

  def isSetback = processingState.isInstanceOf[OrderProcessingState.Setback]

  def isBlacklisted = processingState == OrderProcessingState.Blacklisted

  def isSuspended = obstacles contains OrderObstacle.Suspended
}

object OrderOverview extends OrderView.Companion[OrderOverview] {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat

  implicit val jsonFormat: RootJsonFormat[OrderOverview] = lazyRootFormat(jsonFormat10(apply))

  implicit val ordering: Ordering[OrderOverview] = Ordering by { o ⇒ (o.orderKey.jobChainPath, o.nodeId, o.orderKey.id) }

  final class Statistics(val orderOverviews: immutable.Seq[OrderOverview]) {
    def count = orderOverviews.size
    lazy val inProcessCount = orderOverviews count { _.processingState.isInstanceOf[OrderProcessingState.InTaskProcess] }
    lazy val suspendedCount = orderOverviews count { _.isSuspended }
    lazy val blacklistedCount = orderOverviews count { _.isBlacklisted }
  }
}
