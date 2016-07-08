package com.sos.scheduler.engine.data.order

import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
final case class OrderOverviewCollection(orderOverviews: immutable.Seq[OrderOverview]) {

  def size = orderOverviews.size
  lazy val inProcessCount = orderOverviews count { _.taskId.isDefined }
  lazy val suspendedCount = orderOverviews count { _.isSuspended }
  lazy val blacklistedCount = orderOverviews count { _.isBlacklisted }
}
