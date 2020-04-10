package com.sos.jobscheduler.common.async.synchronizer

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Closer.syntax._
import com.sos.jobscheduler.base.utils.Closer.withCloser
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.scalautil.Futures.blockingThreadFuture
import com.sos.jobscheduler.common.scalautil.Futures.implicits.RichFutures
import org.scalatest.FreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class OwnActorSynchronizerTest extends FreeSpec {

  "test" in {
    withCloser { implicit closer =>
      val actorSystem = newActorSystem("ShellProcessTaskTest") withCloser { _.terminate() }
      import actorSystem.dispatcher
      val synchronizer =
        new OwnActorSynchronizer[Int] {
          protected def actorRefFactory = actorSystem
        }
        .closeWithCloser
      val synchronizedFuture: (=> Int) => Future[Int] = synchronizer
      val numbers = 1 to 100
      @volatile var critical = false
      val futureFutures: IndexedSeq[Future[Future[Int]]] =
        for (i <- numbers) yield blockingThreadFuture {
          synchronizedFuture {
            assert(!critical)
            critical = true
            sleep(10.ms)
            assert(critical)
            critical = false
            i
          }
        }
      assert((futureFutures map { _ flatMap identity } await 60.s) == numbers)

      synchronizer.close()
      intercept[IllegalStateException] { synchronizer { 7 } }
    }
  }
}
