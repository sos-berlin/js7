package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.utils.HasKey
import com.sos.scheduler.engine.data.jobchain.{JobChainPath, NodeId}
import com.sos.scheduler.engine.data.order.OrderPersistentState._
import com.sos.scheduler.engine.data.scheduler.ClusterMemberId
import java.time.Instant

final case class OrderPersistentState(
  jobChainPath: JobChainPath,
  orderId: OrderId,
  distributedNextTimeOption: Option[Instant],
  occupyingClusterIdOption: Option[ClusterMemberId],
  priority: Int,
  ordering: Int,
  nodeIdOption: Option[NodeId],
  initialNodeIdOption: Option[NodeId],
  title: String,
  creationTimestampOption: Option[Instant],
  modificationTimestampOption: Option[Instant],
  payloadXmlOption: Option[String],
  runtimeXmlOption: Option[String],
  xmlOption: Option[String])

extends HasKey {

  type Key = OrderKey

  def key = OrderKey(jobChainPath, orderId)
  //Compiles, but is wrongly typed (Joda vs Java Time): def isBlacklisted = distributedNextTimeOption contains BlacklistDatabaseDistributedNextTime
  def isBlacklisted = distributedNextTimeOption map { _.toEpochMilli } contains BlacklistDatabaseDistributedNextTime.toEpochMilli
}

object OrderPersistentState {
  private val NowDatabaseDistributedNextTime         = Instant.parse("2000-01-01T00:00:00Z")        // Auftrag ist verteilt und ist sofort ausführbar
  private val NeverDatabaseDistributedNextTime       = Instant.parse("3111-11-11T00:00:00Z")        // Auftrag ist verteilt, hat aber keine Startzeit (weil z.B. suspendiert)
  private val BlacklistDatabaseDistributedNextTime   = Instant.parse("3111-11-11T00:01:00Z")        // Auftrag ist auf der schwarzen Liste
  private val ReplacementDatabaseDistributedNextTime = Instant.parse("3111-11-11T00:02:00Z")        // <order replacement="yes">
}
