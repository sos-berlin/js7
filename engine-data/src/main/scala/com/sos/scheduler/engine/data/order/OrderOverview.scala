package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.lazyRootFormat
import com.sos.scheduler.engine.data.filebased.FileBasedState
import com.sos.scheduler.engine.data.jobchain.{NodeId, NodeKey}
import com.sos.scheduler.engine.data.order.OrderProcessingState.OccupiedByClusterMember
import com.sos.scheduler.engine.data.queries.QueryableOrder
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
  orderProcessingState: OrderProcessingState,
  historyId: Option[OrderHistoryId] = None,
  obstacles: Set[OrderObstacle] = Set(),
  startedAt: Option[Instant] = None,
  nextStepAt: Option[Instant] = None)
extends OrderView with QueryableOrder {

  def orderKey: OrderKey = path

  def nodeKey: NodeKey = NodeKey(orderKey.jobChainPath, nodeId)

  def isSetback = processingStateClass == classOf[OrderProcessingState.Setback]

  def isBlacklisted = processingStateClass == OrderProcessingState.Blacklisted.getClass

  def isSuspended = obstacles contains OrderObstacle.Suspended

  def occupyingClusterMemberId = orderProcessingState match {
    case o: OccupiedByClusterMember ⇒ Some(o.clusterMemberId)
    case _ ⇒ None
  }
}

object OrderOverview extends OrderView.Companion[OrderOverview] {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat
  implicit val jsonFormat: RootJsonFormat[OrderOverview] = lazyRootFormat(jsonFormat9(apply))

  implicit val ordering: Ordering[OrderOverview] = Ordering by { o ⇒ (o.orderKey.jobChainPath, o.nodeId, o.orderKey.id) }

  final class Statistics(val orderOverviews: immutable.Seq[OrderOverview]) {
    def count = orderOverviews.size
    lazy val inProcessCount = orderOverviews count { _.processingStateClass == classOf[OrderProcessingState.InTaskProcess] }
    lazy val suspendedCount = orderOverviews count { _.isSuspended }
    lazy val blacklistedCount = orderOverviews count { _.isBlacklisted }
  }
}
