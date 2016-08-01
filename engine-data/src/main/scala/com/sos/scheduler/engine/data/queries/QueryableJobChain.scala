package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.jobchain.JobChainPath
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
trait QueryableJobChain {
  def path: JobChainPath
  def isDistributed: Boolean
}

object QueryableJobChain {
  @TestOnly
  final case class ForTest(
    path: JobChainPath,
    isDistributed: Boolean = false)
  extends QueryableJobChain
}
