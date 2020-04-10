package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.data.master.MasterId
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchRegisterTest extends AnyFreeSpec
{
  private val clusterWatchRegister = new ClusterWatchRegister(Scheduler.global)

  "ClusterWatchRegister" in {
    val aMasterId = MasterId("A")
    val bMasterId = MasterId("B")

    val aClusterWatch = clusterWatchRegister.apply(aMasterId).await(99.s)
    val bClusterWatch = clusterWatchRegister.apply(bMasterId).await(99.s)
    assert(aClusterWatch ne bClusterWatch)

    assert(clusterWatchRegister.apply(aMasterId).await(99.s) eq aClusterWatch)
    assert(clusterWatchRegister.apply(bMasterId).await(99.s) eq bClusterWatch)

    assert(clusterWatchRegister.tryRead(aMasterId).await(99.s).get eq aClusterWatch)
  }
}
