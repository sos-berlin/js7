package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.NodeId
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
trait QueryableJobNode {
  def jobChain: QueryableJobChain
  def nodeId: NodeId
  def jobPath: JobPath
}

object QueryableJobNode {
  @TestOnly
  final case class ForTest(
    jobChain: QueryableJobChain,
    nodeId: NodeId,
    jobPath: JobPath)
  extends QueryableJobNode
}
