package js7.core.cluster.watch

import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.controller.ControllerId
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchRegisterTest extends OurTestSuite
{
  private val clusterWatchRegister = new ClusterWatchRegister(Scheduler.traced)

  "ClusterWatchRegister" in {
    val aControllerId = ControllerId("A")
    val bControllerId = ControllerId("B")

    val aClusterWatch = clusterWatchRegister.apply(aControllerId).await(99.s)
    val bClusterWatch = clusterWatchRegister.apply(bControllerId).await(99.s)
    assert(aClusterWatch ne bClusterWatch)

    assert(clusterWatchRegister.apply(aControllerId).await(99.s) eq aClusterWatch)
    assert(clusterWatchRegister.apply(bControllerId).await(99.s) eq bClusterWatch)

    assert(clusterWatchRegister.tryRead(aControllerId).await(99.s).get eq aClusterWatch)
  }
}
