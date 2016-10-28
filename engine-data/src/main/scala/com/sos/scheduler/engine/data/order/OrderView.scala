package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.common.HasViewCompanion
import com.sos.scheduler.engine.data.filebased.HasPath
import com.sos.scheduler.engine.data.jobchain.NodeKey
import com.sos.scheduler.engine.data.scheduler.ClusterMemberId

/**
  * @author Joacim Zschimmer
  */
trait OrderView extends HasPath {
  def orderKey: OrderKey

  private[engine] def occupyingClusterMemberId: Option[ClusterMemberId]

  private[engine] def orderProcessingState: OrderProcessingState

  private[engine] def nodeKey: NodeKey

  final def orderProcessingStateClass = orderProcessingState.getClass
}

object OrderView extends HasViewCompanion.WithKnownSubtypes[OrderView] {
  protected val subtypes: Subtypes = Set(OrderOverview, OrderDetailed)
}
