package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.base.HasKey
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.OrderPersistentState._
import com.sos.scheduler.engine.data.scheduler.ClusterMemberId
import java.time.Instant
import org.joda.time.ReadableInstant

final case class OrderPersistentState(
  jobChainPath: JobChainPath,
  orderId: OrderId,
  distributedNextTimeOption: Option[ReadableInstant],
  occupyingClusterIdOption: Option[ClusterMemberId],
  priority: Int,
  ordering: Int,
  stateOption: Option[OrderState],
  initialStateOption: Option[OrderState],
  title: String,
  creationTimestampOption: Option[ReadableInstant],
  modificationTimestampOption: Option[ReadableInstant],
  payloadXmlOption: Option[String],
  runtimeXmlOption: Option[String],
  xmlOption: Option[String])

extends HasKey[OrderKey] {

  def key = OrderKey(jobChainPath, orderId)
  //Compiles, but is wrongly typed (Joda vs Java Time): def isOnBlacklist = distributedNextTimeOption contains BlacklistDatabaseDistributedNextTime
  def isOnBlacklist = distributedNextTimeOption map { _.getMillis } contains BlacklistDatabaseDistributedNextTime.toEpochMilli
}

object OrderPersistentState {
  private val NowDatabaseDistributedNextTime         = Instant.parse("2000-01-01T00:00:00Z")        // Auftrag ist verteilt und ist sofort ausführbar
  private val NeverDatabaseDistributedNextTime       = Instant.parse("3111-11-11T00:00:00Z")        // Auftrag ist verteilt, hat aber keine Startzeit (weil z.B. suspendiert)
  private val BlacklistDatabaseDistributedNextTime   = Instant.parse("3111-11-11T00:01:00Z")        // Auftrag ist auf der schwarzen Liste
  private val ReplacementDatabaseDistributedNextTime = Instant.parse("3111-11-11T00:02:00Z")        // <order replacement="yes">
}
