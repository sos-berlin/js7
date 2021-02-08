package js7.core.cluster

import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.controller.ControllerId
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
